# Presentación técnica COMAP

Deck web estático implementado con Quarto + RevealJS.

## Requisitos

- Quarto instalado (`quarto --version`)
- Node.js + npm (solo para scripts convenientes)

## Desarrollo local

Desde la raíz del repo:

```bash
npm run dev
```

Esto abre preview con recarga automática para `presentation/slides.qmd`.

## Build local

```bash
npm run build
```

Salida publicada en `presentation/_site/`.

## Deploy en Netlify

`netlify.toml` en raíz ya está configurado para:

- Build command: `quarto render presentation/slides.qmd --to revealjs`
- Build command: `XDG_CACHE_HOME=/tmp quarto render presentation/slides.qmd --to revealjs`
- Publish directory: `presentation/_site`

Flujo futuro de actualización:

1. Editar `presentation/slides.qmd`
2. Commit + push a GitHub
3. Netlify reconstruye y publica automáticamente
