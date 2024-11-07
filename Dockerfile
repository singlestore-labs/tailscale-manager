FROM haskell:9.6-slim-bullseye AS builder

# Copy source code
WORKDIR /app
COPY . .

# Build static binary, code from https://hasufell.github.io/posts/2024-04-21-static-linking.html
RUN cabal update
RUN cabal build -j --enable-executable-static exe:tailscale-manager
RUN mkdir out/ && cp $(cabal -v0 list-bin exe:tailscale-manager) out/

# Copy the binary to a new image, to keep the final image lightweight
FROM alpine:3.20 AS runtime

COPY --from=builder /app/out/tailscale-manager /
CMD ["/tailscale-manager"]
