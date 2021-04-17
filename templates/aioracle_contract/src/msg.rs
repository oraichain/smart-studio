use cosmwasm_std::CustomQuery;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
pub struct EntryPoint {
    pub url: String,
    pub headers: Option<Vec<String>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
pub struct InitMsg {
    pub dsources: Vec<EntryPoint>,
    pub tcases: Vec<EntryPoint>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum HandleMsg {
    SetDataSources { dsources: Vec<EntryPoint> },
    SetTestCases { tcases: Vec<EntryPoint> },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum QueryMsg {
    Get {
        dsource: EntryPoint,
        input: String,
    },
    Test {
        tcase: EntryPoint,
        input: String,
        output: String,
    },
    GetDataSources {},
    GetTestCases {},
    // all logics must go through Oracle AI module instead of smart contract to avoid gas price problem
    Aggregate {
        results: Vec<String>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "snake_case")]
/// An implementation of QueryRequest::Custom to show this works and can be extended in the contract
pub enum SpecialQuery {
    Fetch {
        url: String,
        body: String,
        method: String,
        headers: Vec<String>,
    },
}
impl CustomQuery for SpecialQuery {}
