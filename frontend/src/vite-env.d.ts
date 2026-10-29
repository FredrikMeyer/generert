/// <reference types="vite/client" />

declare module "virtual:published-images" {
  const images: {
    name: string;
    description: string;
    published: string;
    url: string;
  }[];
  export default images;
}
