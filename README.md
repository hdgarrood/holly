# holly

Holly is an IRC bot, designed for the #purescript channel.

Current capabilities:

* `@pursuit <search query>`, for example:
  * `@pursuit id` should give you back `Prelude.id :: forall a. a -> a`
  * `@pursuit (a -> b) -> List a -> List b` should give you back `Prelude.map :: forall f. (Functor f) => (a -> b) -> f a -> f b`

That's all for now. If you have any other suggestions, please feel free to open
issues!

## where did the name come from?

<https://en.wikipedia.org/wiki/Holly_%28Red_Dwarf%29>
