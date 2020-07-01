namespace Prelude

type Lens<'Inner,'Outer> = {
        getf : 'Outer -> 'Inner
        setf : 'Outer -> 'Inner -> 'Outer
    }

[<RequireQualifiedAccess>]
module Lens =

    let getWith (x: 'Outer) ({getf:f;setf:_}:Lens<'Inner,'Outer>) : 'Inner =
        f x

    let setWith (x: 'Outer) (i: 'Inner) ({getf:_;setf:f}:Lens<'Inner,'Outer>) : 'Outer =
        (f x i)

    let compose ({getf:get1;setf:set1}:Lens<'Inner,'Middle>) ({getf:get2;setf:set2}:Lens<'Middle,'Outer>) : Lens<'Inner,'Outer> =
        {getf = (fun o -> (get1 (get2 o)); setf = (fun o i -> (set2 (set1 i (get2 o)) o)}


[<AutoOpen>]
module Utils =
