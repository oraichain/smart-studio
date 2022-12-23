use js_sys::Uint8Array;
use wasm_bindgen::prelude::*;

// this method use macro to copy fixed size array
fn from_bytes(bytes: &[u8], len: Option<u32>) -> Uint8Array {
    let buffer = Uint8Array::new_with_length(len.unwrap_or(bytes.len() as u32));
    buffer.copy_from(bytes);
    buffer
}

#[wasm_bindgen]
pub fn groth16_verify(
    input: Uint8Array,
    proof: Uint8Array,
    vk: Uint8Array,
) -> Result<bool, JsValue> {
    cosmwasm_crypto::groth16_verify(&input.to_vec(), &proof.to_vec(), &vk.to_vec())
        .map_err(|err| JsValue::from_str(&err.to_string()))
}

#[wasm_bindgen]
pub fn curve_hash(input: Uint8Array) -> Uint8Array {
    from_bytes(&cosmwasm_crypto::curve_hash(&input.to_vec()), None)
}

#[wasm_bindgen]
pub struct Poseidon {
    poseidon: cosmwasm_crypto::Poseidon,
}

#[wasm_bindgen]
impl Poseidon {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Poseidon {
        Poseidon {
            poseidon: cosmwasm_crypto::Poseidon::new(),
        }
    }

    pub fn hash(&self, inputs: Vec<Uint8Array>) -> Result<Uint8Array, JsValue> {
        // create a copy value using let
        let inputs: Vec<Vec<u8>> = inputs.iter().map(|item| item.to_vec()).collect();
        // then create a vector of references
        let mut array: Vec<&[u8]> = vec![];
        for input in inputs.iter() {
            array.push(input);
        }

        self.poseidon
            .hash(&array)
            .map(|item| from_bytes(&item, None))
            .map_err(|err| JsValue::from_str(&err.to_string()))
    }
}

// cargo test --release --package cosmwasm-vm-js-zk --lib --target wasm32-unknown-unknown -- tests::test_zk
#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    // Test poseidon
    const COMMITMENT: &str = "84d6bdcfd953993012f08970d9c9b472d96114b4edc69481968cafc07877381c";

    // Test groth16 verify
    const PUBLIC_INPUT:&str="b7a08d5962f1dc5778f9f2385c291ce6f76f9b075e028e6500f327203bebc61f5219025541ad12054aaf6c0ee8c5ae09f23b693068a9cc80fd960f666cd65121e0fb95195eb23c3c65d7df44a7adf83871ec0380189d3e757417765d51ae0d2a";
    const PROOF : &str="220db2e21ce3a4cb15bc70bdea9a40bfaf17bf236b8a05a298506d1b7fe2c4aeb62795a4dc410cc768fbcf0956155e4a71fb6785c6914d3b6ea9de5fa7b4992591ffbaf98c58474cd062ab9a813fa05a17aef87479a4e5c4d8e8b5e44b0d01869241daef38dd52074a0677564e890e05761ac36c6c9ff23d8356b9ad6d9aeb2f";
    const VK:&str="a8d29ea40629be762f2a12bda7cc45b998a34c43a96c4c6744c8c7a900e8f80a5eece6fa5771489cb0306f499ad91a33d0159f29786332d782db870a0980440aa1225ad23c0e476c7d36b796e6a9b50240841b4be955d13ea54dd8da01128e0cf92094b459ff882780abdf3e1784c06df6c85c0006fd7f2597e3e9052d9215274f08fc4f94dc8129f29a578dc17f5ea60ea85d2c88a78294b792dbf2fa8d30973b80ca6b567463b690b8b3a8f70eb6468227358e5f316eb8150a92152b753519c4ca827fec17f7283d15228767b56b736ec9498f39fe5b511a8af503b63a19970400000000000000b9c86bbe3e5ef3490d5db478be0a7933934e4b5a148e2c01602b475c83e2500891dc44a6fe2f331da9b66f3562398a762677f7b7c95d3a82a9698e0cf6212128d6eb27a3f11abb52a98a509bf1502f0947e9ab9c1d72a086140c0d316866d624c22a69f8dfb5957e35d8d3d350b3c83ee95e8897c7b76d41881683d2561cd919";

    #[wasm_bindgen_test]
    fn test_zk() {
        let curve_hash = curve_hash(from_bytes(&hex::decode(COMMITMENT).unwrap(), None));
        console_log!("curve_hash: {:?}", hex::encode(curve_hash.to_vec()));

        let input = from_bytes(&hex::decode(PUBLIC_INPUT).unwrap(), None);
        let proof = from_bytes(&hex::decode(PROOF).unwrap(), None);
        let vk = from_bytes(&hex::decode(VK).unwrap(), None);

        let verified = groth16_verify(input, proof, vk);
        console_log!("verified: {:?}", verified);

        let poseidon = Poseidon::new();
        let commitment_hash = from_bytes(&hex::decode(COMMITMENT).unwrap(), None);
        let poseidon_hash = poseidon
            .hash(vec![commitment_hash.clone(), commitment_hash])
            .map(|item| hex::encode(item.to_vec()));

        console_log!("poseidon_hash: {:?}", poseidon_hash);
    }
}
