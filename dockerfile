###############################################################################
# 1) Build stage – BLST + cardano‑swaps (GHC 9.4.8 already present)
###############################################################################
FROM haskell:9.6.4-buster AS build
# ghcr.io/haskell/ghc images come with: ghc, cabal‑install‑3.14.2, curl,
# git, gcc, etc.

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp-dev libsodium-dev libsecp256k1-dev zlib1g-dev libtinfo-dev \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# ── build & install BLST (creates libblst-any.pc) ─────────────────────────────
RUN git clone --depth 1 https://github.com/supranational/blst.git /tmp/blst \
 && cd /tmp/blst && ./build.sh \
 && install -Dm644 libblst.a /usr/local/lib/libblst.a \
 && install -Dm644 bindings/blst.h /usr/local/include/blst/blst.h \
 && mkdir -p /usr/local/lib/pkgconfig \
 && printf 'prefix=/usr/local\nexec_prefix=${prefix}\nlibdir=${exec_prefix}/lib\nincludedir=${prefix}/include\n\nName: libblst-any\nVersion: 0.3\nLibs: -L${libdir} -lblst\nCflags: -I${includedir}\n' \
    >/usr/local/lib/pkgconfig/libblst-any.pc

# make sure pkg‑config sees it everywhere
RUN mkdir -p /usr/lib/pkgconfig /usr/lib/x86_64-linux-gnu/pkgconfig && \
    cp /usr/local/lib/pkgconfig/libblst-any.pc /usr/lib/pkgconfig/ && \
    cp /usr/local/lib/pkgconfig/libblst-any.pc /usr/lib/x86_64-linux-gnu/pkgconfig/

# ── copy repo & local constraints ────────────────────────────────────────────
WORKDIR /src
COPY . .

RUN sed -i '/^[[:space:]]*with-compiler[[:space:]]*:/d' cabal.project

RUN { \
      echo 'package cardano-crypto-class'; \
      echo '  flags: -external-blst'; \
      echo ''; \
      echo 'package digest'; \
      echo '  flags: -pkg-config'; \
    } >> cabal.project.local

# ---------------------------------------------------------------------------
# Use /usr/local/cabal-cache instead of /tmp for the writable cache location
# ---------------------------------------------------------------------------
ENV XDG_CACHE_HOME=/usr/local/cabal-cache
RUN mkdir -p $XDG_CACHE_HOME

# --- build cardano-swaps -----------------------------------------------------
RUN cabal update && \
    cabal build cardano-swaps:exe:cardano-swaps \
      --disable-tests --disable-benchmarks -j


###############################################################################
# 2) Runtime stage – tiny image with runtime libs only
###############################################################################
FROM ubuntu:24.04
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
      libgmp10 libsodium23 zlib1g libtinfo6 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

COPY --from=build /src/dist-newstyle/build/*/*/cardano-swaps-*/x/cardano-swaps/build/cardano-swaps/cardano-swaps /usr/local/bin/

ENTRYPOINT ["cardano-swaps"]
CMD ["--help"]
