module EnumHelpers

let inline ToEnum<'t> v =
    if Enum.IsDefined(typeof<'t>, v) |> not then None
    let res : 't = v |> LanguagePrimitives.EnumOfValue
    res |> Some
