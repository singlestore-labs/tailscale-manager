FROM haskell:9.6-slim-bullseye AS builder

# Install dependencies first, to cache them
WORKDIR /app
COPY tailscale-manager.cabal .
RUN cabal update && cabal build -j --only-dependencies

# Copy source code
COPY . .

# Build static binary, code from https://hasufell.github.io/posts/2024-04-21-static-linking.html
RUN cabal build -j --enable-executable-static exe:tailscale-manager
RUN mkdir out/ && cp $(cabal -v0 list-bin exe:tailscale-manager) out/

# Copy the binary to a new image, to keep the final image lightweight
FROM alpine:3.20 AS runtime

COPY --from=builder /app/out/tailscale-manager /
CMD ["/tailscale-manager"]
