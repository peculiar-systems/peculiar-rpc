declare module "*.md" {
  export const meta: { readonly [key: string]: string };
  export const html: string;
}
