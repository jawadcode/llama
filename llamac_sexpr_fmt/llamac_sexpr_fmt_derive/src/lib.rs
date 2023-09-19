extern crate proc_macro;

use std::fmt::Pointer;

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

#[derive(Clone, PartialEq)]
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

fn impl_struct_unnamed_fields(
    name: &str,
    fields: FieldsUnnamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    struct Field {
        fmt_type: FmtType,
        idx: usize,
    }

    let mut fields = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            Ok(Field {
                fmt_type: get_fmt_type(&field.attrs, idents)?,
                idx,
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    // TODO: Split fields into grops of metadata, display, debug, and normal

    let split = itertools::partition(&mut fields, |field| field.fmt_type == FmtType::Metadata);
    let (metadata, fields) = fields.split_at(split);

    let metadata_sep = quote! { f.write_str(", ")?; };
    let metadata_quote: Vec<TokenStream> = Itertools::intersperse(
        metadata.iter().map(|field| {
            let idx = Index::from(field.idx);
            quote! {
                self.#idx.fmt(f)?;
            }
        }),
        metadata_sep,
    )
    .collect();
    let comment = if !metadata_quote.is_empty() {
        quote! { f.write_str("    ; ")?; }
    } else {
        quote! {}
    };

    let fields = fields.iter().map(|field| {
        let idx = Index::from(field.idx);
        let fmt_call = match field.fmt_type {
            FmtType::Metadata => unreachable!(),
            FmtType::Display => quote! {
                std::fmt::Display::fmt(&self.#idx, f)?;
            },
            FmtType::Debug => quote! {
                std::fmt::Debug::fmt(&self.#idx, f)?;
            },
            FmtType::Normal => quote! {
                self.#idx.sexp_fmt(f, depth + 1)?;
            },
        };
        quote! {
            f.write_str("\n    ")?;
            for _ in 0..depth {
                f.write_str("    ")?;
            }
            #fmt_call
        }
    });

    Ok(quote! {
        f.write_str(concat!("(", #name))?;
        #comment
        #(#metadata_quote)*
        #(#fields)*
        f.write_char(')')?;
    })
}

fn impl_struct_named_fields(
    name: &str,
    fields: FieldsNamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    struct Field<'a> {
        fmt_type: FmtType,
        ident: &'a Ident,
    }

    let mut fields = fields
        .named
        .iter()
        .map(|field| {
            Ok(Field {
                fmt_type: get_fmt_type(&field.attrs, idents)?,
                ident: field.ident.as_ref().unwrap(),
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let split = itertools::partition(&mut fields, |field| field.fmt_type == FmtType::Metadata);
    let (metadata, fields) = fields.split_at(split);

    let metadata_sep = quote! { f.write_str(", ")?; };
    let metadata_quote: Vec<TokenStream> = Itertools::intersperse(
        metadata.iter().map(|Field { ident, .. }| {
            quote! {
                std::fmt::Display::fmt(&self.#ident, f)?;
            }
        }),
        metadata_sep,
    )
    .collect();
    let comment = if !metadata_quote.is_empty() {
        quote! { f.write_str("    ; ")?; }
    } else {
        TokenStream::new()
    };

    let fields = fields.iter().map(
        |Field {
             fmt_type, ident, ..
         }| {
            let fmt_call = match fmt_type {
                FmtType::Metadata => unreachable!(),
                FmtType::Display => quote! {
                    f.write_char(' ')?;
                    std::fmt::Display::fmt(&self.#ident, f)?;
                },
                FmtType::Debug => quote! {
                    f.write_char(' ')?;
                    std::fmt::Debug::fmt(&self.#ident, f)?;
                },
                FmtType::Normal => quote! {
                    f.write_str("\n        ")?;
                    for _ in 0..depth {
                        f.write_str("    ")?;
                    }
                    self.#ident.sexp_fmt(f, depth + 2)?;
                },
            };
            quote! {
                f.write_str("\n    ")?;
                for _ in 0..depth {
                    f.write_str("    ")?;
                }
                f.write_char(':')?;
                f.write_str(stringify!(#ident))?;
                #fmt_call
            }
        },
    );

    Ok(quote! {
        f.write_str(concat!("(", #name))?;
        #comment
        #(#metadata_quote)*
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
    struct VariantData {
        ident: Ident,
        fields: TokenStream,
    }
    let variants = variants
        .map(|variant| {
            let name = ident.to_string() + "::" + &variant.ident.to_string().to_case(Case::Kebab);
            let ident = variant.ident;
            let fields = match variant.fields {
                Fields::Named(named) => impl_struct_named_fields(&name, named, idents)?,
                Fields::Unnamed(unnamed) => impl_struct_unnamed_fields(&name, unnamed, idents)?,
                Fields::Unit => quote! { f.write_str(#name)?; },
            };
            Ok(VariantData { ident, fields })
        })
        .collect::<syn::Result<Vec<_>>>()?;
    let arms = variants.into_iter().map(|VariantData { ident, fields }| {
        quote! {
            Self::#ident => {
                #fields
            }
        }
    });
    Ok(quote! {
        match self {
            #(#arms),*
        };
    })
}

/* de-duplicate all of this crap code */

fn impl_enum_named(
    name: &str,
    fields: FieldsNamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    Ok(quote! {})
}

fn impl_enum_unnamed(
    name: &str,
    fields: FieldsUnnamed,
    idents: Idents<'_>,
) -> syn::Result<TokenStream> {
    #[derive(Clone)]
    struct Field {
        fmt_type: FmtType,
        ident: Ident,
    }

    let fields_ord = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            Ok(Field {
                fmt_type: get_fmt_type(&field.attrs, idents)?,
                ident: format_ident!("field{idx}"),
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let mut fields = fields_ord.clone();

    let split = itertools::partition(&mut fields, |field| field.fmt_type == FmtType::Metadata);
    let (metadata, fields) = fields.split_at(split);

    Ok(quote! {})
}
