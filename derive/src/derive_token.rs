use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens as _};
use syn::Result;

unzip_n::unzip_n!(3);

pub(crate) fn derive_token_impl(input: syn::ItemEnum) -> Result<TokenStream> {
    let pratt = if let Some(pratt) = parse_enum_attrs(input.attrs)? {
        pratt
    } else {
        super::name()?
    };

    let vis = input.vis;
    let parsed = input
        .variants
        .into_iter()
        .map(|mut variant| {
            let attrs = variant.attrs;
            variant.attrs = Vec::new();
            let fields = variant.fields;
            variant.fields = syn::Fields::Unit;
            let (payload, span) = parse_variant_attrs(attrs, &variant)?;
            Ok((variant, fields, payload, span))
        })
        .collect::<Result<Vec<_>>>()?;

    let variants = parsed.iter().map(|(v, _, _, _)| v);
    let type_enum = quote! {
        #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
        #vis enum __TokenType {
            #(#variants),*
        }
    };

    let ident = &input.ident;
    let (typ_arms, payload_arms, span_arms) = parsed.iter().map(|(variant, fields, payload, span)| {
        let (pat, binders, ty) = parse_fields(fields)?;
        let span = span.as_ref().map_or_else(
            || quote!(#pratt::span::Span::UNKNOWN),
            |s| quote!(#s)
        );
        Ok((
            quote!(#ident::#variant #pat => __TokenType::#variant),
            quote!(#ident::#variant #pat => String::from((|it: #ty| #payload)(#binders))),
            quote!(#ident::#variant #pat => (|it: #ty| #span)(#binders))
        ))
    }).collect::<Result<Vec<_>>>()?.into_iter().unzip_n::<Vec<_>, Vec<_>, Vec<_>>();
    let trait_impl = quote! {
        impl #pratt::Token for #ident {
            type Type = __TokenType;

            fn typ(&self) -> Self::Type {
                match self {
                    #(#typ_arms),*
                }
            }

            fn payload(&self) -> String {
                match self {
                    #(#payload_arms),*
                }
            }

            fn span(&self) -> #pratt::span::Span {
                match self {
                    #(#span_arms),*
                }
            }
        }
    };

    Ok(quote! {
        const _: () = {
            #type_enum
            #trait_impl

            impl #pratt::Prototypical for #ident {
                type Prototype = #ident;
                fn prototype(&self) -> &Self::Prototype {
                    &self
                }
            }
        };
    })
}

fn parse_enum_attrs(attrs: Vec<syn::Attribute>) -> Result<Option<TokenStream>> {
    let mut crate_ = None;
    for attr in attrs
        .into_iter()
        .filter(|attr| attr.path().is_ident("pratt"))
    {
        match attr.parse_args::<syn::Meta>()? {
            syn::Meta::NameValue(syn::MetaNameValue {
                path, value, ..
            }) if path.is_ident("crate") => {
                if crate_.is_some() {
                    return Err(syn::Error::new_spanned(
                        path,
                        "duplicate crate override",
                    ));
                }
                crate_ = Some(value);
            }
            meta => {
                return Err(syn::Error::new_spanned(
                    meta,
                    "unrecognised pratt argument",
                ))
            }
        }
    }
    Ok(crate_.map(syn::Expr::into_token_stream))
}

fn parse_variant_attrs(
    attrs: Vec<syn::Attribute>,
    varaint: &syn::Variant,
) -> Result<(syn::Expr, Option<syn::Expr>)> {
    let mut payload_expr = None;
    let mut span_expr = None;
    for attr in attrs
        .into_iter()
        .filter(|attr| attr.path().is_ident("pratt"))
    {
        let contents = attr.parse_args_with(syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated)?;
        for meta in contents {
            match meta {
                syn::Meta::NameValue(syn::MetaNameValue {
                    path,
                    value,
                    ..
                }) => {
                    if path.is_ident("payload") {
                        if payload_expr.is_some() {
                            return Err(syn::Error::new_spanned(
                                path,
                                "duplicate payload definition",
                            ));
                        }
                        payload_expr = Some(value);
                    } else if path.is_ident("span") {
                        if span_expr.is_some() {
                            return Err(syn::Error::new_spanned(
                                path,
                                "duplicate span definition",
                            ));
                        }
                        span_expr = Some(value);
                    } else {
                        return Err(syn::Error::new_spanned(
                            path,
                            "unrecognised pratt argument",
                        ));
                    }
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        meta,
                        "unrecognised pratt argument",
                    ))
                }
            }
        }
    }
    let payload_expr = payload_expr.ok_or_else(|| {
        syn::Error::new_spanned(
            varaint,
            "variant is missing payload definition",
        )
    })?;
    Ok((payload_expr, span_expr))
}

fn parse_fields(
    fields: &syn::Fields,
) -> Result<(TokenStream, TokenStream, TokenStream)> {
    match fields {
        syn::Fields::Unit => Ok((quote!(), quote!(()), quote!(()))),
        syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed, .. }) => {
            let wildcards = unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| format_ident!("_{i}"));
            let binders = wildcards.clone();
            let types = unnamed.iter().map(|f| &f.ty);
            Ok((
                quote!((#(#wildcards),*)),
                quote!((#(#binders,)*)),
                quote!((#(&#types,)*)),
            ))
        }
        syn::Fields::Named(named) => Err(syn::Error::new_spanned(
            named,
            "named fields aren't supported",
        )),
    }
}
