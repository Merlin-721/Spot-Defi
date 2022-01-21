//! # Unique Assets Implementation: Commodities
//!
//! This pallet exposes capabilities for managing unique assets, also known as
//! non-fungible tokens (NFTs).
//!
//! - [`pallet_commodities::Trait`](./trait.Trait.html)
//! - [`Calls`](./enum.Call.html)
//! - [`Errors`](./enum.Error.html)
//! - [`Events`](./enum.RawEvent.html)
//!
//! ## Overview
//!
//! Assets that share a common metadata structure may be created and distributed
//! by an asset admin. Asset owners may burn assets or transfer their
//! ownership. Configuration parameters are used to limit the total number of a
//! type of asset that may exist as well as the number that any one account may
//! own. Assets are uniquely identified by the hash of the info that defines
//! them, as calculated by the runtime system's hashing algorithm.
//!
//! This pallet implements the [`UniqueAssets`](./nft/trait.UniqueAssets.html)
//! trait in a way that is optimized for assets that are expected to be traded
//! frequently.
//!
//! ### Dispatchable Functions
//!
//! * [`mint`](./enum.Call.html#variant.mint) - Use the provided commodity info
//!   to create a new commodity for the specified user. May only be called by
//!   the commodity admin.
//!
//! * [`burn`](./enum.Call.html#variant.burn) - Destroy a commodity. May only be
//!   called by commodity owner.
//!
//! * [`transfer`](./enum.Call.html#variant.transfer) - Transfer ownership of
//!   a commodity to another account. May only be called by current commodity
//!   owner.

#![cfg_attr(not(feature = "std"), no_std)]

use codec::FullCodec;
use frame_support::{
    decl_error, decl_event, decl_module, decl_storage, dispatch, ensure,
    Hashable,
    traits::{EnsureOrigin, Get},
};
use frame_system::ensure_signed;
use sp_runtime::traits::{Hash, Member};
use sp_std::{cmp::Eq, fmt::Debug, vec::Vec};
use frame_support::debug::print;

pub use crate::spotdefinft::SpotDefiNFT;
use frame_support::dispatch::DispatchError;

pub mod spotdefinft;
//use frame_support::traits::Instance;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

pub trait Trait<I = DefaultInstance>: frame_system::Trait {

    type Event: From<Event<Self, I>> + Into<<Self as frame_system::Trait>::Event>;

    ///nft info
    type TokenInfo: Hashable + Member + Debug + Default + FullCodec + Ord;

}

pub type TokenId<T> = <T as frame_system::Trait>::Hash;
pub type TokenAddress = Vec<u8>;

decl_storage! {
    trait Store for Module<T: Trait<I>, I: Instance = DefaultInstance> as Commodity {
        /// The total number of this type of commodity that exists.
        TotalSpotDefiNFTs get(fn total_spotdefi_nfts): u128 = 0;
        /// A mapping from an account to a list of all of the commodities of this type that are owned by it.
        SpotDefiForAccount get(fn spotdefi_for_account): map hasher(blake2_128_concat) T::AccountId => Vec<(TokenId<T>, u16)>;
        /// A mapping from a commodity ID to the account that owns it.
        AccountsForSpotDefi get(fn accounts_for_spotdefi): map hasher(identity) TokenId<T> => Vec<(T::AccountId, u16)>;
        /// mapping for adress
        AddressForSpotDefi get(fn address_for_spotdefi): map hasher(identity) TokenId<T> => TokenAddress;
    }

}

decl_event!(
    pub enum Event<T, I = DefaultInstance>
    where
        AccountId = <T as frame_system::Trait>::AccountId,
        TokenId = <T as frame_system::Trait>::Hash,
    {
        /// The Spotdefy NFT has been minted and distributed to the account.
        MintSpotDefy(TokenId, AccountId),
        /// Units of this SpotDefy NFT have been transferred to this account
        TransferSpotDefy(AccountId, TokenId, u16),
        /// Units of this token owned by thus account
        UnitsOwnedSpotDefy(u16, TokenId, AccountId),
        /// address of toek
        AddressOfSpotDefy(Vec<u8>, TokenId),
        ///play event
        PlayEvent(Vec<u8>, Vec<(AccountId, u16)>),
    }
);

decl_error! {
    pub enum Error for Module<T: Trait<I>, I: Instance> {
        // Thrown when there is an attempt to mint a duplicate commodity.
        CommodityExists,
        // Thrown when there is an attempt to burn or transfer a nonexistent commodity.
        NonexistentCommodity,
        // Thrown when someone who is not the owner of a commodity attempts to transfer or burn it.
        NotCommodityOwner,
        // Thrown when the commodity admin attempts to mint a commodity and the maximum number of this
        // type of commodity already exists.
        TooManyCommodities,
        // Thrown when an attempt is made to mint or transfer a commodity to an account that already
        // owns the maximum number of this type of commodity.
        TooManyCommoditiesForAccount,
        //Thrown when trying to transfer from account but not owner
        NotSpotDefiNFTOwner,
        //Thrown when trying to transfer but not sufficient units owned
        NotEnoughUnits,
    }
}

decl_module! {
    pub struct Module<T: Trait<I>, I: Instance = DefaultInstance> for enum Call where origin: T::Origin {
        type Error = Error<T, I>;
        fn deposit_event() = default;

        ///MInt a new Spotdefi NFT
        #[weight = 10_000]
        pub fn mintSpotdefyNft(origin, owner_account: T::AccountId, token_info: T::TokenInfo) -> dispatch::DispatchResult {

            let token_id = <Self as SpotDefiNFT<_>>::mintSpotdefyNft(&owner_account, token_info)?;
            Self::deposit_event(RawEvent::MintSpotDefy(token_id, owner_account.clone()));
            Ok(())
        }

        #[weight = 5_000]
        pub fn transferUnits(origin, dest_account: T::AccountId, token_id: TokenId<T>, units: u16) -> dispatch::DispatchResult {
            let who = ensure_signed(origin)?;
            <Self as SpotDefiNFT<_>>::transferUnits(&who, &dest_account, &token_id, units)?;
            Self::deposit_event(RawEvent::TransferSpotDefy(dest_account.clone(), token_id.clone(), units));
            Ok(())
        }

        ///addressofnft
        #[weight = 5_000]
        pub fn addressOfaToken(origin, token_id: TokenId<T>) -> dispatch::DispatchResult {
            let address = <Self as SpotDefiNFT<_>>::addressOfaToken(&token_id)?;
            Self::deposit_event(RawEvent::AddressOfSpotDefy(address, token_id.clone()));
            Ok(())
        }

        /// play nft here
        #[weight = 5_000]
        pub fn play(origin, token_id: TokenId<T>) -> dispatch::DispatchResult {
            let playResult = <Self as SpotDefiNFT<_>>::play(&token_id)?;
            Self::deposit_event(RawEvent::PlayEvent(playResult.0, playResult.1));
            Ok(())
        }
    }
}


impl<T: Trait<I>, I: Instance> SpotDefiNFT<T::AccountId> for Module<T, I> {

    type SpotDefTokenId = TokenId<T>;
    type SpotDefTokenInfo = T::TokenInfo;
    type SpotDefAddress = TokenAddress;

    fn total() -> u128 {
        Self::total()
    }

    fn mintSpotdefyNft(
        owner_account: &T::AccountId,
        token_info: <T as Trait<I>>::TokenInfo,
    ) -> dispatch::result::Result<TokenId<T>, dispatch::DispatchError> {
        let token_id = T::Hashing::hash_of(&token_info);

        TotalSpotDefiNFTs::<I>::mutate(|total| *total += 1);

        let mut tokenvec = Vec::new();
        tokenvec.push((&owner_account, 1000));
        AccountsForSpotDefi::<T, I>::insert(&token_id, tokenvec);
        AddressForSpotDefi::<T,I>:: insert(&token_id, "http:://assetlocation.com".as_bytes());
        if SpotDefiForAccount::<T, I>::contains_key(&owner_account) {
            let mut ownerVec = SpotDefiForAccount::<T, I>::get(&owner_account);
            ownerVec.push((token_id, 1000));
        } else {
            let mut ownerVec = Vec::new();
            ownerVec.push((token_id, 1000));
            SpotDefiForAccount::<T, I>::insert(&owner_account, ownerVec);
        }
        Ok(token_id)
    }

    fn play( token_id: &TokenId<T>,
    ) -> dispatch::result::Result<(TokenAddress, Vec<(T::AccountId, u16)>), dispatch::DispatchError> {
        let accts = AccountsForSpotDefi::<T, I>::get(token_id);
        let address = AddressForSpotDefi::<T, I>::get(token_id);
        Ok((address, accts))
    }

    fn addressOfaToken(
        token_id: &TokenId<T>,
    ) -> Result<TokenAddress, DispatchError> {
        Ok(AddressForSpotDefi::<T, I>::get(&token_id))
    }

    fn transferUnits(owner_account: &T::AccountId,
                     dest_account: &T::AccountId,
                     token_id: &TokenId<T>,
                     units: u16,
    ) -> dispatch::DispatchResult {
        ensure!(
            SpotDefiForAccount::<T, I>::contains_key(&owner_account),
            Error::<T, I>::NotSpotDefiNFTOwner
        );
        let mut unitsHeld: u16 = 0;
        let mut ownerVec = SpotDefiForAccount::<T, I>::get(&owner_account);
        for pair in ownerVec.iter() {
            if pair.0 == *token_id {
                unitsHeld = pair.1
            }
        }
        ensure!(
            unitsHeld >= units,
            Error::<T, I>::NotEnoughUnits
        );
        //Remove units from old first
        if SpotDefiForAccount::<T, I>::contains_key(&dest_account) {
            let mut newOwnerVec = SpotDefiForAccount::<T, I>::get(&dest_account);

            let trasnferred = false;
            let mut unitsHeldOld: u16 = 0;
            for pair in ownerVec.iter_mut() {
                if pair.0 == *token_id {
                    unitsHeldOld = pair.1;
                    let unitsHeldNew = unitsHeldOld + units;
                    pair.1 = unitsHeldNew;
                }
                if !trasnferred {
                    //Holder did not have existing units, so create new entry
                    newOwnerVec.push((token_id.clone(), units));
                }
            }
            SpotDefiForAccount::<T, I>::insert(&dest_account, newOwnerVec);

        }

        else {
            let mut newOwnerVec = Vec::new();
            newOwnerVec.push((*token_id, units))
        }
        for pair in ownerVec.iter_mut() {
            if pair.0 == *token_id {
                pair.1 = unitsHeld - units
            }
        }

        if AccountsForSpotDefi::<T, I>::contains_key(&token_id) {
            let mut accounts = AccountsForSpotDefi::<T, I>::get(&token_id);
            let mut transf = false;
            for acc in accounts.iter_mut() {
                if acc.0 == *dest_account {
                    let unitsHeldOld = acc.1;
                    let unitsHeldNew = unitsHeldOld + units;
                    acc.1 = unitsHeldNew;
                    transf = true;
                }
                if acc.0 == *owner_account {
                    acc.1 = acc.1 - units;
                }
            }
            if !transf{
                accounts.push((dest_account.clone(), units))
            }
            AccountsForSpotDefi::<T, I>::insert(&token_id, accounts);

        }
        Ok(())
    }
}
