#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapScriptFile="${tmpDir}twoWaySwap.plutus" # This is used to create the swap address.
ownerPubKeyFile="$HOME/wallets/01Stake.vkey"
swapAddrFile="${tmpDir}twoWaySwap.addr"
swapDatumFile="${tmpDir}swapDatum.json"
beaconRedeemerFile="${tmpDir}twoWaySwapBeaconRedeemer.json"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
beaconScriptPreprodTestnetRef="115c9ebb9928b8ec6e0c9d1420c43421cfb323639dd9fdcf1e7155e73bec13c5#1"
# beaconScriptSize=4707

# Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

# Create the swap address.
echo -n "Creating the swap address... "
cardano-cli conway address build \
  --payment-script-file $swapScriptFile \
  --stake-verification-key-file $ownerPubKeyFile \
  --testnet-magic 1 \
  --out-file $swapAddrFile

echo "$(cat $swapAddrFile)"

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

asset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

asset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
asset1Beacon="${beaconPolicyId}.${asset1BeaconName}"
asset2Beacon="${beaconPolicyId}.${asset2BeaconName}"

# Get the mint beacon redeemer.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --first-price '1 / 1000000' \
  --second-price 2000000 \
  --out-file $swapDatumFile

# Create the transaction.
echo "Building the transaction..."
cardano-cli conway transaction build \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#2 \
  --tx-in 338945b8cc6d4c49aa0b94452b2f99c29c4b68efa89fcda8ed691e6fc81c97b0#0 \
  --tx-in 44f58115ad9738de64bb5624b495a2e7abddfdbe055df474d7d980af1244d64e#0 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${asset1Beacon} + 1 ${asset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --mint "1 ${pairBeacon} + 1 ${asset1Beacon} + 1 ${asset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --change-address "$(cat $HOME/wallets/01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
