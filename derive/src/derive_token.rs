use std::mem;

use proc::{
    meta::{Optional, Required},
    quote::{quote, ToTokens},
    syn::{parse_quote, Error, Expr, Fields, Ident, Visibility},
    util::{field_names_and_types, EnumFieldMatcher},
    ItemEnum, Path, Result, TokenStream,
};

pub(crate) struct DeriveToken {
    crate_: Path,
    vis: Visibility,
    ident: Ident,
    variants: Vec<Variant>,
}

impl DeriveToken {
    pub(crate) fn new(
        crate_: Path,
        ItemEnum {
            attrs: _,
            vis,
            enum_token: _,
            ident,
            generics,
            brace_token: _,
            variants,
        }: ItemEnum,
    ) -> Result<Self> {
        if !generics.params.is_empty() {
            return Err(Error::new_spanned(
                generics,
                "#[derive(Token)] doesn't support generic enums",
            ));
        }

        let variants = variants
            .into_iter()
            .map(|v| Variant::new(&crate_, v))
            .collect::<Result<_>>()?;

        Ok(Self {
            crate_,
            vis,
            ident,
            variants,
        })
    }

    fn type_enum(&self) -> TypeEnum<'_> {
        TypeEnum::new(&self.vis, &self.variants)
    }

    fn trait_impl(&self) -> TraitImpl<'_> {
        TraitImpl::new(&self.crate_, &self.ident, &self.variants)
    }
}

impl ToTokens for DeriveToken {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let crate_ = &self.crate_;
        let ident = &self.ident;
        let type_enum = self.type_enum();
        let trait_impl = self.trait_impl();
        tokens.extend(quote! {
            const _: () = {
                #type_enum
                #trait_impl

                impl #crate_::Prototypical for #ident {
                    type Prototype = #ident;
                    fn prototype(&self) -> &Self::Prototype {
                        &self
                    }
                }
            };
        });
    }
}

struct Variant {
    payload: Expr,
    span: Expr,
    fields: Fields,
    variant: proc::syn::Variant,
}

impl Variant {
    fn new(crate_: &Path, mut variant: proc::syn::Variant) -> Result<Self> {
        let attrs = mem::take(&mut variant.attrs);
        let parser: (Required<Expr>, Optional<Expr>) =
            (Required::new("payload"), Optional::new("span"));
        let (payload, span) = proc::meta::parse_attrs(parser, "pratt", &attrs)?;
        let fields = mem::replace(&mut variant.fields, Fields::Unit);
        Ok(Self {
            payload,
            span: span
                .unwrap_or_else(|| parse_quote!(#crate_::span::Span::UNKNOWN)),
            fields,
            variant,
        })
    }
}

struct TypeEnum<'a> {
    vis: &'a Visibility,
    variants: &'a [Variant],
}

impl<'a> TypeEnum<'a> {
    fn new(vis: &'a Visibility, variants: &'a [Variant]) -> Self {
        Self { vis, variants }
    }
}

impl ToTokens for TypeEnum<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vis = self.vis;
        let variants = self.variants.iter().map(|v| &v.variant);
        tokens.extend(quote! {
            #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
            #vis enum __TokenType {
                #(#variants),*
            }
        });
    }
}

struct TraitImpl<'a> {
    crate_: &'a Path,
    ident: &'a Ident,
    variants: &'a [Variant],
}

impl<'a> TraitImpl<'a> {
    fn new(
        crate_: &'a Path,
        ident: &'a Ident,
        variants: &'a [Variant],
    ) -> Self {
        Self {
            crate_,
            ident,
            variants,
        }
    }
}

impl ToTokens for TraitImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let crate_ = &self.crate_;
        let ident = &self.ident;
        let variants =
            self.variants.iter().map(|v| &v.variant).collect::<Vec<_>>();
        let matchers = self
            .variants
            .iter()
            .map(|v| EnumFieldMatcher(&v.fields))
            .collect::<Vec<_>>();
        let (names, types): (Vec<_>, Vec<_>) = self
            .variants
            .iter()
            .map(|v| {
                let (names, types): (Vec<_>, Vec<_>) =
                    field_names_and_types(&v.fields).unzip();
                (names, types)
            })
            .unzip();
        let payloads = self.variants.iter().map(|v| &v.payload);
        let spans = self.variants.iter().map(|v| &v.span);
        tokens.extend(quote! {
            impl #crate_::Token for #ident {
                type Type = __TokenType;

                fn typ(&self) -> Self::Type {
                    match self {
                        #(Self::#variants #matchers => __TokenType::#variants),*
                    }
                }

                fn payload(&self) -> String {
                    match self {
                        #(Self::#variants #matchers => String::from((|it: (#(&#types),*)| #payloads)((#(#names),*)))),*
                    }
                }

                fn span(&self) -> #crate_::span::Span {
                    match self {
                        #(Self::#variants #matchers => (|it: (#(&#types),*)| #spans)((#(#names),*))),*
                    }
                }
            }
        });
    }
}
