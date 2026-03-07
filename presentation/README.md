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

- Build command: vacío (sitio pre-renderizado en el repo)
- Publish directory: `presentation/_site`

Flujo futuro de actualización:

1. Editar `presentation/slides.qmd`
2. Commit + push a GitHub
3. Netlify publica el contenido actualizado de `presentation/_site`
