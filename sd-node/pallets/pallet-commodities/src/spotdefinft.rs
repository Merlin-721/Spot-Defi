//! # SpotDefiNFT Interface
//!
//! Implementation of NFT for representing a sharable asset.
//! For group project Distributed Ledgers - Spot-defi project
//! Some boilerplate - but not logic, from
//! https://github.com/danforbes/pallet-nft
//!
//!
//! ## Overview
//!
//! A spotdefy NFT is divided into 1000 units.  On minting, the NFT is assigned to an account.
//! After creation, units can be trasnfered to other accounts to share ownership
//!

use frame_support::{
    dispatch::{DispatchError, DispatchResult, result::Result},
    traits::Get,
};
use sp_std::vec::Vec;

/// Account IDs represent substrate IDs
/// Token IDs must be unique.  In practice for spotdefy these represent a hash of metadata about a
/// musical work, but this is not enforced
pub trait SpotDefiNFT<AccountId> {
    // Number of equal(fungible) units the token can be divided into
    const DIVISIBILITY: u16 = 1000;

    /// The type used to identify unique assets.
    type SpotDefTokenId;
    type SpotDefTokenInfo;
    type SpotDefAddress;
    /// The total number of tokens
    fn total() -> u128;

    fn mintSpotdefyNft(
        owner_account: &AccountId,
        token_info: Self::SpotDefTokenInfo,
    ) -> Result<Self::SpotDefTokenId, DispatchError>;

    fn play(
        token_id: &Self::SpotDefTokenId,
    ) -> Result<(Self::SpotDefAddress, Vec<(AccountId, u16)>), DispatchError>;

    fn addressOfaToken(
        token_id: &Self::SpotDefTokenId,
    ) -> Result<Self::SpotDefAddress, DispatchError>;

    fn transferUnits(owner_account: &AccountId,
                     dest_account: &AccountId,
                     token_id: &Self::SpotDefTokenId,
                     units: u16,
    ) -> DispatchResult;
}
