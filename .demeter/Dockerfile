###############################################################################
# Demeter build image – Haskell 9.4.8 + BLST + cardano‑swaps
###############################################################################
FROM haskell:9.4.8

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp-dev libsodium-dev libsecp256k1-dev zlib1g-dev libtinfo-dev git \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# ---- BLST (static) ----------------------------------------------------------
RUN git clone --depth 1 https://github.com/supranational/blst /tmp/blst \
 && cd /tmp/blst && ./build.sh \
 && install -Dm644 libblst.a /usr/local/lib/libblst.a \
 && install -Dm644 bindings/blst.h /usr/local/include/blst/blst.h \
 && printf 'prefix=/usr/local\nexec_prefix=${prefix}\nlibdir=${exec_prefix}/lib\nincludedir=${prefix}/include\n\nName: libblst-any\nVersion: 0.3\nLibs: -L${libdir} -lblst\nCflags: -I${includedir}\n' \
      >/usr/local/lib/pkgconfig/libblst-any.pc

# ---- project ----------------------------------------------------------------
WORKDIR /workspace      # Demeter mounts your repo here
COPY . .

# wipe any compiler pins that fight GHC 9.4
RUN sed -i '/^[[:space:]]*with-compiler[[:space:]]*:/d' cabal.project || true

# local constraints (vector < 0.13, disable external‑blst, older ouroboros libs)
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
