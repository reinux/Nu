namespace Nugen
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type NugenPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetNugen Splash world)
             ("Title", fun world -> Game.SetNugen Title world)
             ("Credits", fun world -> Game.SetNugen Credits world)
             ("Gameplay", fun world ->
                let world = Simulants.Gameplay.SetGameplay Gameplay.initial world
                let world = Game.SetNugen Gameplay world
                world)]