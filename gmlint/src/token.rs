/// Different types of token available to GML.
#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
    DirectiveAllow,
    DirectiveWarn,
    DirectiveOption,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBox,
    RightBox,
    Dot,
    VarDecl,
    Identifier,
    Comment,
    Space,
    Tab,
    EoL,
    EoF,
    BoF,
    Other,
}
