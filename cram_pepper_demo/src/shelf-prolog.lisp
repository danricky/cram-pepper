(in-package :pepper-demo)

(def-fact-group product-type-predicates () 

  (<- (is-of-type ?product ?type)
    (is-type ?product ?type)
    (product ?product)
    (type ?type))
 
  (<- (product cereal))
  (<- (product milk))
  (<- (product bowl)) 

  (<- (type :breakfast-cereal))
  (<- (type :milk))     

  (<- (is-type cereal :breakfast-cereal))
  (<- (is-type milk :milk))) 

(def-fact-group shelf-product-predicates () 

  (<- (is-on-shelf ?object ?shelf)
    (is-on ?object ?shelf)
    (object ?object)
    (shelf ?shelf))

  (<- (is-on-shelf ?object)
    (is-on ?object ?shelf)
    (shelf ?shelf))

  (<- (shelf shelf-one-level-1))
  (<- (shelf shelf-one-level-2))
  (<- (shelf shelf-one-level-3))
  (<- (shelf shelf-one-level-4))
  (<- (shelf shelf-one-level-5))

  (<- (shelf shelf-two-level-1))
  (<- (shelf shelf-two-level-2))
  (<- (shelf shelf-two-level-3))
  (<- (shelf shelf-two-level-4))
  (<- (shelf shelf-two-level-5))

  (<- (object cereal))
  (<- (object somat))
  (<- (object milk))
  (<- (object bowl))         
  (<- (object denkmit))         

  (<- (is-on cereal shelf-one-level-4))
  (<- (is-on cereal shelf-two-level-3))
  (<- (is-on cereal shelf-one-level-2))

  (<- (is-on somat shelf-one-level-2))
  (<- (is-on somat shelf-one-level-3))
  (<- (is-on somat shelf-one-level-4))

  (<- (is-on milk shelf-one-level-4))
  (<- (is-on milk shelf-two-level-4))

  (<- (is-on bowl shelf-two-level-1))

  (<- (is-on denkmit shelf-two-level-2))) 

(def-fact-group shelf-location-predicates () 

  (<- (is-located-at ?shelfs ?location)
    (is-at ?shelfs ?location)
    (location ?location)
    (shelfs ?shelfs))

  (<- (shelfs shelf-one-level-1))
  (<- (shelfs shelf-one-level-2))
  (<- (shelfs shelf-one-level-3))
  (<- (shelfs shelf-one-level-4))
  (<- (shelfs shelf-one-level-5))

  (<- (shelfs shelf-two-level-1))
  (<- (shelfs shelf-two-level-2))
  (<- (shelfs shelf-two-level-3))
  (<- (shelfs shelf-two-level-4))
  (<- (shelfs shelf-two-level-5))

  (<- (location :|DM-SHELVES.shelf_1_level_1_link|))
  (<- (location :|DM-SHELVES.shelf_1_level_2_link|))
  (<- (location :|DM-SHELVES.shelf_1_level_3_link|))
  (<- (location :|DM-SHELVES.shelf_1_level_4_link|))
  (<- (location :|DM-SHELVES.shelf_1_level_5_link|))          

  (<- (location :|DM-SHELVES.shelf_2_level_1_link|))
  (<- (location :|DM-SHELVES.shelf_2_level_2_link|))
  (<- (location :|DM-SHELVES.shelf_2_level_3_link|))
  (<- (location :|DM-SHELVES.shelf_2_level_4_link|))
  (<- (location :|DM-SHELVES.shelf_2_level_5_link|)) 

  (<- (is-at shelf-one-level-1 :|DM-SHELVES.shelf_1_level_1_link| ))
  (<- (is-at shelf-one-level-2 :|DM-SHELVES.shelf_1_level_2_link| ))
  (<- (is-at shelf-one-level-3 :|DM-SHELVES.shelf_1_level_3_link| ))
  (<- (is-at shelf-one-level-4 :|DM-SHELVES.shelf_1_level_4_link| ))
  (<- (is-at shelf-one-level-5 :|DM-SHELVES.shelf_1_level_5_link| ))

  (<- (is-at shelf-two-level-1 :|DM-SHELVES.shelf_2_level_1_link| ))
  (<- (is-at shelf-two-level-2 :|DM-SHELVES.shelf_2_level_2_link| ))
  (<- (is-at shelf-two-level-3 :|DM-SHELVES.shelf_2_level_3_link| ))
  (<- (is-at shelf-two-level-4 :|DM-SHELVES.shelf_2_level_4_link| )) 
  (<- (is-at shelf-two-level-5 :|DM-SHELVES.shelf_2_level_5_link| ))) 