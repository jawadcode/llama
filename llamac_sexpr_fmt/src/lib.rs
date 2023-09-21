use std::fmt;

pub trait SExpFmt {
    fn sexp_fmt(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result;
}

#[cfg(test)]
mod tests {
    use std::fmt::Display;

    use crate::SExpFmt;
    use llamac_sexpr_fmt_derive::SExpFmt;

    struct Span {
        start: usize,
        end: usize,
    }

    impl Display for Span {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "span: {}..{}", self.start, self.end)
        }
    }

    #[test]
    fn derive_tuple_struct() {
        #[derive(SExpFmt)]
        struct TupleStruct(#[metadata] Span, #[debug] String, #[display] usize);

        let test = TupleStruct(
            Span {
                start: 123,
                end: 234,
            },
            "testing".to_string(),
            2,
        );

        assert_eq!(
            test.to_string(),
            r#"
(tuple-struct    ; span: 123..234
    "testing"
    2)"#
            .trim_start()
        )
    }

    #[test]
    fn derive_tuple_struct_nested() {
        #[derive(SExpFmt)]
        struct TupleStruct1(#[metadata] Span, TupleStruct2, #[display] usize);

        #[derive(SExpFmt)]
        struct TupleStruct2(#[metadata] Span, #[debug] String, #[display] usize);

        let test = TupleStruct1(
            Span {
                start: 123,
                end: 234,
            },
            TupleStruct2(
                Span {
                    start: 345,
                    end: 456,
                },
                "testing".to_string(),
                2,
            ),
            3,
        );

        assert_eq!(
            test.to_string(),
            r#"
(tuple-struct-1    ; span: 123..234
    (tuple-struct-2    ; span: 345..456
        "testing"
        2)
    3)"#
            .trim_start()
        )
    }

    #[test]
    fn derive_named_struct() {
        #[derive(SExpFmt)]
        struct NamedStruct {
            #[metadata]
            span: Span,
            #[debug]
            first: String,
            #[display]
            second: usize,
        }

        let test = NamedStruct {
            span: Span {
                start: 123,
                end: 234,
            },
            first: "testing".to_string(),
            second: 2,
        };

        assert_eq!(
            test.to_string(),
            r#"
(named-struct    ; span: 123..234
    :first "testing"
    :second 2)"#
                .trim_start()
        )
    }

    #[test]
    fn derive_named_struct_nested() {
        #[derive(SExpFmt)]
        struct NamedStruct1 {
            #[metadata]
            span: Span,
            first: NamedStruct2,
            #[display]
            second: usize,
        }

        #[derive(SExpFmt)]
        struct NamedStruct2 {
            #[metadata]
            span: Span,
            #[debug]
            first: String,
            #[display]
            second: usize,
        }

        let test = NamedStruct1 {
            span: Span {
                start: 123,
                end: 234,
            },
            first: NamedStruct2 {
                span: Span {
                    start: 345,
                    end: 456,
                },
                first: "testing".to_string(),
                second: 2,
            },
            second: 3,
        };

        assert_eq!(
            test.to_string(),
            r#"
(named-struct-1    ; span: 123..234
    :first
        (named-struct-2    ; span: 345..456
            :first "testing"
            :second 2)
    :second 3)"#
                .trim_start()
        )
    }

    #[test]
    fn derive_enum_units() {
        #[derive(SExpFmt)]
        enum EnumOfUnits {
            First,
            Second,
        }

        let test1 = EnumOfUnits::First;

        assert_eq!(
            test1.to_string(),
            r"
enum-of-units::first"
                .trim_start()
        );

        let test2 = EnumOfUnits::Second;

        assert_eq!(
            test2.to_string(),
            r"
enum-of-units::second"
                .trim_start()
        );
    }

    #[test]
    fn derive_enum_unnamed() {
        #[derive(SExpFmt)]
        enum EnumOfUnnamed {
            First(#[metadata] Span, #[debug] String, #[display] usize),
            Second(#[metadata] Span, #[debug] String, #[display] usize),
        }

        let test1 = EnumOfUnnamed::First(
            Span {
                start: 123,
                end: 234,
            },
            "eggs".to_string(),
            2,
        );
        assert_eq!(
            test1.to_string(),
            r#"
(enum-of-unnamed::first    ; span: 123..234
    "eggs"
    2)"#
            .trim_start()
        );

        let test2 = EnumOfUnnamed::Second(
            Span {
                start: 345,
                end: 456,
            },
            "toast".to_string(),
            4,
        );
        assert_eq!(
            test2.to_string(),
            r#"
(enum-of-unnamed::second    ; span: 345..456
    "toast"
    4)"#
            .trim_start()
        );
    }

    #[test]
    fn derive_enum_unnamed_nested() {
        #[derive(SExpFmt)]
        enum EnumOfUnnamed1 {
            First(#[metadata] Span, EnumOfUnnamed2, #[display] usize),
            Second(#[metadata] Span, EnumOfUnnamed2, #[display] usize),
        }

        #[derive(SExpFmt)]
        enum EnumOfUnnamed2 {
            Third(#[metadata] Span, #[debug] String, #[display] usize),
            Fourth(#[metadata] Span, #[debug] String, #[display] usize),
        }

        let test1 = EnumOfUnnamed1::First(
            Span {
                start: 123,
                end: 234,
            },
            EnumOfUnnamed2::Third(
                Span {
                    start: 345,
                    end: 456,
                },
                "testing".to_string(),
                1,
            ),
            2,
        );
        assert_eq!(
            test1.to_string(),
            r#"
(enum-of-unnamed-1::first    ; span: 123..234
    (enum-of-unnamed-2::third    ; span: 345..456
        "testing"
        1)
    2)"#
            .trim_start()
        );

        let test2 = EnumOfUnnamed1::Second(
            Span {
                start: 567,
                end: 678,
            },
            EnumOfUnnamed2::Fourth(
                Span {
                    start: 789,
                    end: 890,
                },
                "testing".to_string(),
                3,
            ),
            4,
        );
        assert_eq!(
            test2.to_string(),
            r#"
(enum-of-unnamed-1::second    ; span: 567..678
    (enum-of-unnamed-2::fourth    ; span: 789..890
        "testing"
        3)
    4)"#
            .trim_start()
        );
    }

    #[test]
    fn derive_enum_named() {
        #[derive(SExpFmt)]
        enum EnumOfUnnamed {
            First {
                #[metadata]
                span: Span,
                #[debug]
                first: String,
                #[display]
                second: usize,
            },
            Second {
                #[metadata]
                span: Span,
                #[debug]
                first: String,
                #[display]
                second: usize,
            },
        }

        let test1 = EnumOfUnnamed::First {
            span: Span {
                start: 123,
                end: 234,
            },
            first: "eggs".to_string(),
            second: 2,
        };
        assert_eq!(
            test1.to_string(),
            r#"
(enum-of-unnamed::first    ; span: 123..234
    :first "eggs"
    :second 2)"#
                .trim_start()
        );

        let test2 = EnumOfUnnamed::Second {
            span: Span {
                start: 345,
                end: 456,
            },
            first: "toast".to_string(),
            second: 4,
        };
        assert_eq!(
            test2.to_string(),
            r#"
(enum-of-unnamed::second    ; span: 345..456
    :first "toast"
    :second 4)"#
                .trim_start()
        );
    }

    #[test]
    fn derive_enum_named_nested() {
        #[derive(SExpFmt)]
        enum EnumOfUnnamed1 {
            First {
                #[metadata]
                span: Span,
                first: EnumOfUnnamed2,
                #[display]
                second: usize,
            },
            Second {
                #[metadata]
                span: Span,
                first: EnumOfUnnamed2,
                #[display]
                second: usize,
            },
        }

        #[derive(SExpFmt)]
        enum EnumOfUnnamed2 {
            Third {
                #[metadata]
                span: Span,
                #[debug]
                first: String,
                #[display]
                second: usize,
            },
            Fourth {
                #[metadata]
                span: Span,
                #[debug]
                first: String,
                #[display]
                second: usize,
            },
        }

        let test1 = EnumOfUnnamed1::First {
            span: Span {
                start: 123,
                end: 234,
            },
            first: EnumOfUnnamed2::Third {
                span: Span {
                    start: 345,
                    end: 456,
                },
                first: "eggs".to_string(),
                second: 2,
            },
            second: 3,
        };
        assert_eq!(
            test1.to_string(),
            r#"
(enum-of-unnamed-1::first    ; span: 123..234
    :first
        (enum-of-unnamed-2::third    ; span: 345..456
            :first "eggs"
            :second 2)
    :second 3)"#
                .trim_start()
        );

        let test2 = EnumOfUnnamed1::Second {
            span: Span {
                start: 567,
                end: 678,
            },
            first: EnumOfUnnamed2::Fourth {
                span: Span {
                    start: 789,
                    end: 890,
                },
                first: "toast".to_string(),
                second: 4,
            },
            second: 5,
        };
        assert_eq!(
            test2.to_string(),
            r#"
(enum-of-unnamed-1::second    ; span: 567..678
    :first
        (enum-of-unnamed-2::fourth    ; span: 789..890
            :first "toast"
            :second 4)
    :second 5)"#
                .trim_start()
        );
    }
}
