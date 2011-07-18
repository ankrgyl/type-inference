structure Context :> CONTEXT =
struct
  structure Map = SplayMapFn(struct
                               type ord_key = int
                               val compare = Int.compare
                             end)

  type 'a context = 'a Map.map
  val empty = Map.empty

  fun bind G v = Map.insert (G, Map.numItems G, v)
  fun lookup G x = Map.find (G, (Map.numItems G)-x-1)

  fun search p G =
      (* This does a bunch of lookups but splay trees are blazingly fast
       * for in order lookups, so... *)
      let val size = Map.numItems G
          fun search' i =
              if i >= size then NONE
              else 
                  let val x = valOf (lookup G i)
                  in if p (i, x) then SOME (i, x)
                     else search' (i+1)
                  end
      in search' 0 end

  fun toList G = rev (Map.listItems G)
end
