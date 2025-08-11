use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as syn::DeriveInput);

    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = {
        for param in input.generics.params.iter_mut() {
            if let syn::GenericParam::Type(ty_param) = param {
                ty_param.bounds.push(syn::parse_quote!(::std::fmt::Debug));
            }
        }
        input.generics.split_for_impl()
    };
    let named_fields = match input.data {
        syn::Data::Struct(data_struct) => match data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named,
            syn::Fields::Unnamed(_) => {
                return syn::Error::new(
                    name.span(),
                    "expected named struct, but found unnamed struct.",
                )
                .to_compile_error()
                .into()
            }
            syn::Fields::Unit => {
                return syn::Error::new(
                    name.span(),
                    "expected named struct, but found union struct.",
                )
                .to_compile_error()
                .into()
            }
        },
        syn::Data::Enum(_) => {
            return syn::Error::new(name.span(), "expected named struct, but found enum.")
                .to_compile_error()
                .into()
        }
        syn::Data::Union(_) => {
            return syn::Error::new(name.span(), "expected named struct, but found union.")
                .to_compile_error()
                .into()
        }
    };

    let name_str = syn::LitStr::new(&name.to_string(), name.span());
    let debug_fields = named_fields.named.iter().map(|f| {
        let field_ident = f.ident.clone().unwrap();
        let field_str = syn::LitStr::new(&field_ident.to_string(), field_ident.span());

        let mut debug_field = None;
        for attr in f.attrs.iter().filter(|attr| attr.path().is_ident("debug")) {
            if let Ok(meta) = attr.meta.require_name_value() {
                if let syn::Expr::Lit(expr_lit) = &meta.value {
                    if let syn::Lit::Str(fmt) = &expr_lit.lit {
                        if debug_field.is_none() {
                            debug_field = Some(fmt.clone());
                            continue;
                        } else {
                            return syn::Error::new(
                                attr.span(),
                                "at most 1 attribute is allowed for each field.",
                            )
                            .to_compile_error()
                            .into();
                        }
                    }
                }
                return syn::Error::new(
                    attr.span(),
                    r##"expected #[debug = "..."] style attribute."##,
                )
                .to_compile_error()
                .into();
            }
        }

        match debug_field {
            Some(fmt) => quote! {
                debug_struct.field(#field_str, &::std::format_args!(#fmt, &self.#field_ident));
            },
            None => quote! {
                debug_struct.field(#field_str, &self.#field_ident);
            },
        }
    });

    let extended = quote! {
        impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                let mut debug_struct = f.debug_struct(#name_str);
                #( #debug_fields )*
                debug_struct.finish()
            }
        }
    };

    extended.into()
}
