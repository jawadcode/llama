extern crate proc_macro;

use convert_case::{Case, Casing};
use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse, spanned::Spanned, Attribute, Data, DataEnum, DataStruct, DeriveInput, Error, Fields,
    FieldsNamed, FieldsUnnamed, Index, Lit, Meta, MetaNameValue, Path, Variant,
};

#[proc_macro_derive(SExpFmt, attributes(name, metadata, display, debug))]
pub fn llamac_sexpr_fmt_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse(input).unwrap();
    impl_sexp_fmt(ast)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

#[derive(Clone, Copy)]
struct Idents<'a> {
    name: &'a Ident,
    metadata: &'a Ident,
    display: &'a Ident,
    debug: &'a Ident,
}

fn impl_sexp_fmt(ast: DeriveInput) -> syn::Result<TokenStream> {
    let ident = &ast.ident;

    let idents = Idents {
        name: &Ident::new("name", Span::call_site()),
        metadata: &Ident::new("metadata", Span::call_site()),
        display: &Ident::new("display", Span::call_site()),
        debug: &Ident::new("debug", Span::call_site()),
    };

    let name = ast
        .attrs
        .iter()
        .map(Attribute::parse_meta)
        .filter_map(Result::ok)
        .find_map(|meta| match meta {
            Meta::NameValue(MetaNameValue {
                path,
                lit: Lit::Str(name),
                ..
            }) if path.is_ident(idents.name) => Some(name),
            _ => None,
        })
        .map(|lit_str| lit_str.value())
        .unwrap_or_else(|| ast.ident.to_string().to_case(Case::Kebab));

    let impl_body = match ast.data {
        Data::Struct(DataStruct { fields, .. }) => match fields {
            Fields::Named(named) => impl_struct_named_fields(&name, named, idents)?,
            Fields::Unnamed(unnamed) => impl_struct_unnamed_fields(&name, unnamed, idents)?,
            Fields::Unit => quote! { f.write_str("#name_ident")?; },
        },
        Data::Enum(DataEnum { variants, .. }) => impl_enum(&name, variants.into_iter(), idents)?,
        Data::Union(u) => {
            return Err(Error::new(
                u.union_token.span,
                "You can't format a union silly",
            ))
        }
    };

    Ok(quote! {
        impl SExpFmt for #ident {
            fn sexp_fmt(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
                use std::fmt::Write;
                #impl_body
                Ok(())
            }
        }

        impl std::fmt::Display for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.sexp_fmt(f, 0)
            }
        }
    })
}

struct FieldsImpl {
    comment: TokenStream,
    metadata: Vec<TokenStream>,
    fields: Vec<TokenStream>,
}

#[derive(Clone, Copy, PartialEq)]
enum FmtType {
    // Use `Display::fmt`
    Metadata,
    // Use `Display::fmt`
    Display,
    // Use `Debug::fmt`
    Debug,
    // Use `SExpFmt::sexp_fmt`
    Normal,
}

trait GenFmtImpl: Sized {
    const DEPTH_ADD: usize = 1;
    const NAMED: bool;

    fn get_fmt_type(&self) -> FmtType;
    fn access(&self) -> TokenStream;
    fn get_field_name(&self) -> TokenStream;

    fn gen_fmt_call(&self) -> TokenStream {
        let access = self.access();
        let add = Self::DEPTH_ADD;
        let (field_indent, space) = if Self::NAMED {
            (
                quote! { f.write_str("\n        ")?; },
                quote! { f.write_char(' ')?; },
            )
        } else {
            (TokenStream::new(), TokenStream::new())
        };
        match self.get_fmt_type() {
            FmtType::Metadata => unreachable!(),
            FmtType::Display => quote! {
                #space
                std::fmt::Display::fmt(&#access, f)?;
            },
            FmtType::Debug => quote! {
                #space
                std::fmt::Debug::fmt(&#access, f)?;
            },
            FmtType::Normal => quote! {
                #field_indent
                #access.sexp_fmt(f, depth + #add)?;
            },
        }
    }

    fn gen_fmt_impl(mut fields: Vec<Self>) -> FieldsImpl {
        let split = itertools::partition(&mut fields, |field| {
            field.get_fmt_type() == FmtType::Metadata
        });
        let (metadata, fields) = fields.split_at(split);

        let metadata_sep = quote! { f.write_str(", ")?; };
        let metadata: Vec<TokenStream> = Itertools::intersperse(
            metadata.iter().map(|field| {
                let access = field.access();
                quote! {
                    #access.fmt(f)?;
                }
            }),
            metadata_sep,
        )
        .collect();
        let comment = if !metadata.is_empty() {
            quote! { f.write_str("    ; ")?; }
        } else {
            TokenStream::new()
        };

        let fields = fields
            .iter()
            .map(|field| {
                let fmt_call = field.gen_fmt_call();
                let field_name = field.get_field_name();
                quote! {
                    f.write_str("\n    ")?;
                    for _ in 0..depth {
                        f.write_str("    ")?;
                    }
                    #field_name
                    #fmt_call
                }
            })
            .collect();

        FieldsImpl {
            comment,
            metadata,
            fields,
        }
    }
}

struct SUField {
    fmt_type: FmtType,
    idx: usize,
}

impl GenFmtImpl for SUField {
    const NAMED: bool = false;

    fn get_fmt_type(&self) -> FmtType {
        self.fmt_type
    }

    fn access(&self) -> TokenStream {
        let idx = Index::from(self.idx);
        quote! { self.#idx }
    }

    fn get_field_name(&self) -> TokenStream {
        TokenStream::new()
    }
}

fn impl_struct_unnamed_fields(
    name: &str,
    fields: FieldsUnnamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    let fields = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            Ok(SUField {
                fmt_type: get_fmt_type(&field.attrs, idents)?,
                idx,
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let FieldsImpl {
        comment,
        metadata,
        fields,
    } = GenFmtImpl::gen_fmt_impl(fields);

    Ok(quote! {
        f.write_str(concat!("(", #name))?;
        #comment
        #(#metadata)*
        #(#fields)*
        f.write_char(')')?;
    })
}

struct SNField<'a> {
    fmt_type: FmtType,
    ident: &'a Ident,
}

impl GenFmtImpl for SNField<'_> {
    const DEPTH_ADD: usize = 2;
    const NAMED: bool = true;

    fn get_fmt_type(&self) -> FmtType {
        self.fmt_type
    }

    fn access(&self) -> TokenStream {
        let Self { ident, .. } = self;
        quote! { self.#ident }
    }

    fn get_field_name(&self) -> TokenStream {
        let Self { ident, .. } = self;
        quote! {
            f.write_char(':')?;
            f.write_str(stringify!(#ident))?;
        }
    }
}

fn impl_struct_named_fields(
    name: &str,
    fields: FieldsNamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    let fields = fields
        .named
        .iter()
        .map(|field| {
            Ok(SNField {
                fmt_type: get_fmt_type(&field.attrs, idents)?,
                ident: field.ident.as_ref().unwrap(),
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let FieldsImpl {
        comment,
        metadata,
        fields,
    } = GenFmtImpl::gen_fmt_impl(fields);

    Ok(quote! {
        f.write_str(concat!("(", #name))?;
        #comment
        #(#metadata)*
        #(#fields)*
        f.write_char(')')?;
    })
}

fn get_fmt_type(attrs: &[Attribute], idents: Idents<'_>) -> syn::Result<FmtType> {
    let attrs: Vec<Path> = attrs
        .iter()
        .filter_map(|attr| {
            attr.parse_meta().ok().and_then(|meta| match meta {
                Meta::Path(path) => Some(path),
                _ => None,
            })
        })
        .collect();

    let metadata = attrs
        .iter()
        .find(|path| path.is_ident(idents.metadata))
        .map(Spanned::span);
    let display = attrs
        .iter()
        .find(|path| path.is_ident(idents.display))
        .map(Spanned::span);
    let debug = attrs
        .iter()
        .find(|path| path.is_ident(idents.debug))
        .map(Spanned::span);

    match (metadata, display, debug) {
        (Some(_), None, None) => Ok(FmtType::Metadata),
        (None, Some(_), None) => Ok(FmtType::Display),
        (None, None, Some(_)) => Ok(FmtType::Debug),
        (None, None, None) => Ok(FmtType::Normal),
        (Some(_), Some(span), _) | (Some(_), None, Some(span)) | (None, Some(_), Some(span)) => Err(Error::new(span, "A field may only be annotated with one of the following 3, or none of them: #[metadata], #[display], #[debug]")),
    }
}

fn impl_enum(
    ident: &str,
    variants: impl Iterator<Item = Variant>,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    let variants = variants
        .map(|variant| {
            let name = ident.to_string() + "::" + &variant.ident.to_string().to_case(Case::Kebab);
            let ident = variant.ident;
            let fields = match variant.fields {
                Fields::Named(named) => impl_variant_named(&name, ident, named, idents)?,
                Fields::Unnamed(unnamed) => impl_variant_unnamed(&name, ident, unnamed, idents)?,
                Fields::Unit => quote! {
                    Self::#ident => f.write_str(#name)?
                },
            };
            Ok(fields)
        })
        .collect::<syn::Result<Vec<_>>>()?;
    Ok(quote! {
        match self {
            #(#variants),*
        };
    })
}

struct VUField {
    fmt_type: FmtType,
    ident: Ident,
}

impl GenFmtImpl for VUField {
    const DEPTH_ADD: usize = 1;

    const NAMED: bool = false;

    fn get_fmt_type(&self) -> FmtType {
        self.fmt_type
    }

    fn access(&self) -> TokenStream {
        let Self { ident, .. } = self;
        quote! { #ident }
    }

    fn get_field_name(&self) -> TokenStream {
        TokenStream::new()
    }
}

fn impl_variant_unnamed(
    name: &str,
    ident: Ident,
    fields: FieldsUnnamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    let (field_names, fields): (Vec<Ident>, Vec<VUField>) = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(idx, field)| -> syn::Result<_> {
            let field_name = format_ident!("e_u_field{idx}");
            Ok((
                field_name.clone(),
                VUField {
                    fmt_type: get_fmt_type(&field.attrs, idents)?,
                    ident: field_name,
                },
            ))
        })
        .process_results(|fields| fields.unzip())?;

    let FieldsImpl {
        comment,
        metadata,
        fields,
    } = GenFmtImpl::gen_fmt_impl(fields);

    Ok(quote! {
        Self::#ident(#(#field_names),*) => {
            f.write_str(concat!("(", #name))?;
            #comment
            #(#metadata)*
            #(#fields)*
            f.write_char(')')?;
        }
    })
}

struct VNField {
    fmt_type: FmtType,
    ident: Ident,
}

impl GenFmtImpl for VNField {
    const DEPTH_ADD: usize = 2;
    const NAMED: bool = true;

    fn get_fmt_type(&self) -> FmtType {
        self.fmt_type
    }

    fn access(&self) -> TokenStream {
        let Self { ident, .. } = self;
        quote! { #ident }
    }

    fn get_field_name(&self) -> TokenStream {
        let Self { ident, .. } = self;
        quote! {
            f.write_char(':')?;
            f.write_str(stringify!(#ident))?;
        }
    }
}

fn impl_variant_named(
    name: &str,
    ident: Ident,
    fields: FieldsNamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    let (field_names, fields): (Vec<Ident>, Vec<VNField>) = fields
        .named
        .iter()
        .map(|field| -> syn::Result<_> {
            // Guaranteed to be named, i.e. have an identifier
            let field_name = field.ident.clone().unwrap();
            Ok((
                field_name.clone(),
                VNField {
                    fmt_type: get_fmt_type(&field.attrs, idents)?,
                    ident: field_name,
                },
            ))
        })
        .process_results(|fields| fields.unzip())?;

    let FieldsImpl {
        comment,
        metadata,
        fields,
    } = GenFmtImpl::gen_fmt_impl(fields);

    Ok(quote! {
        Self::#ident { #(#field_names),* } => {
            f.write_str(concat!("(", #name))?;
            #comment
            #(#metadata)*
            #(#fields)*
            f.write_char(')')?;
        }
    })
}
