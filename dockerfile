###############################################################################
# 1) Build stage – BLST + cardano‑swaps (GHC 9.4.8 already present)
###############################################################################
FROM ghcr.io/haskell/ghc:9.4.8-bullseye AS build
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
      echo 'constraints: vector <0.13'; \
      echo ''; \
      echo 'package cardano-crypto-class'; \
      echo '  flags: -external-blst'; \
      echo ''; \
      echo 'constraints:'; \
      echo '  ouroboros-consensus == 0.10.0.0,'; \
      echo '  ouroboros-network-framework == 0.8.3.0,'; \
      echo '  ouroboros-network-protocols == 0.6.0.0,'; \
      echo '  ouroboros-network-api == 0.6.0.0,'; \
      echo '  ouroboros-network == 0.9.0.0'; \
    } >> cabal.project.local

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
