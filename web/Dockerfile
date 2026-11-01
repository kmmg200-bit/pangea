# Mini App собирает Node, а отдаёт Caddy — он же держит HTTPS перед ботом
# (см. docker/caddy/Caddyfile).
FROM node:22.23.3-alpine AS build
WORKDIR /web
COPY package.json package-lock.json ./
RUN npm ci --no-audit --no-fund
COPY . .
RUN npm run build

FROM caddy:2.11.6-alpine
COPY --from=build /web/dist /srv
