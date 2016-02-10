{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}


{- Números Naturales -}
data N = Zero | Suc N deriving Show

--Suma de naturales.
suma::N->N->N
suma Zero n = n
suma (Suc n) m = Suc (suma n m)

--Producto de naturales.
prod::N->N->N
prod Zero _ = Zero
prod (Suc n) m = suma (prod n m) m

--Potencia de naturales.
pot::N->N->N
pot n Zero = Suc Zero
pot n (Suc Zero) = n
pot n (Suc m) = prod (pot n m) n

{- Números DNat -}
data DNat = Cero | D DNat | U DNat deriving Show


--Para simplificar un DNat.
simplDN :: DNat -> DNat
simplDN Cero  = Cero
simplDN (U n) = U $ simplDN n
simplDN (D n) = f $ D $ simplDN n where
		f (D Cero) = Cero
		f (D n) = D $ simplDN n


--Sucesor de un DNat.
sucDN :: DNat->DNat
sucDN n = simplDN $ suc_aux n where
	  suc_aux Cero  = U Cero
	  suc_aux (D n) = U n
	  suc_aux (U n) = D $ sucDN n


--Predecesor de un número DNat.
predDN :: DNat->DNat
predDN n = simplDN $ suc_aux n where
	  suc_aux Cero  = D Cero
	  suc_aux (U n) = D n
	  suc_aux (D n) = U $ predDN n


--Representación de un número DNat en los números enteros.
dNToZ :: DNat->Int
dNToZ Cero = 0
dNToZ (D n) = 2 * dNToZ n
dNToZ (U n) = 2 * (dNToZ n) + 1

--Suma dos números DNat.
sumaDN :: DNat->DNat->DNat
sumaDN n m = simplDN $ f_suma n m where
		f_suma n Cero = n
		f_suma Cero n  = n
		f_suma (D n) (U m) =  U $ f_suma n m
		f_suma (D n) (D m) =  D $ f_suma n m
		f_suma (U n) (D m) =  U $ f_suma n m
		f_suma (U n) (U m) =  D $ sucDN $ f_suma n m


--Multiplica dos números DNat.
prodDN :: DNat->DNat->DNat
prodDN n m  = simplDN $ prod_aux n m where
       	      prod_f Cero n = Cero
       	      prod_f n Cero = Cero
       	      prod_f (U Cero) n = n
       	      prod_f n (U Cero) = n
       	      prod_f (D n) (D m) = D $ D $ prodDN n m
       	      prod_f (D n) (U m) = D $ sumaDN(D $ prodDN n m) n
       	      prod_f (U n) (D m) = D $ sumaDN(D $ prodDN n m) m
       	      prod_f (U n) (U m) = U $ sumaDN(sumaDN(D $ prodDN n m) n) m

--Transforma un entero positivo a su representación en DNat.
zToDNat :: Int->DNat
zToDNat 0 = Cero
zToDNat n = sucDN $ zToDNat(n-1)


{- Listas -}
--Elimina repeticiones de una lista.
--toSet::Eq a=>[a]->[a]


--Cuenta el número de apariciones de un elemento en una lista.
--cuantas::Eq a=>a->[a]->Int


--Cuentas las apariciones de cada elemento en la lista.
--frec::Eq a=>[a]->[(a,Int)]


--Nos da los elementos que aparecen una sola vez.
--unaVez::Eq a=>[a]->[a]


{- Retos -}
--compress1::String->String


--compress2::String->String


{- Pruebas -}
   --Naturales
--Debe dar: Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero))))))
prueba1 = suma (Suc $ Suc Zero) (suma (Suc $ Suc $ Suc $ Suc Zero) (Suc Zero))

--Debe dar: Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero)))))))
--prueba2 = prod (Suc $ Suc Zero) (prod (Suc $ Suc $ Suc $ Suc Zero) (Suc Zero))

--Debe dar: Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero)))))))
--prueba3 = pot (suma (Suc Zero) (Suc Zero)) (prod (Suc Zero) (Suc $ Suc $ Suc Zero))

   --DNat
--Debe dar: 31
--prueba4 = dNToZ $ sucDN $ sumaDN (D $ D $ U $ U $ D $ D Cero) (predDN $ zToDNat 19)
--Debe dar: 5844
--prueba5 = dNToZ $ sucDN $ sucDN $ prodDN (U $ U $ U $ D $ U $ D $ D Cero) (sumaDN (U $ D $ U $ D $ D Cero) (zToDNat 249))
--Debe dar: 21
--prueba6 = (dNToZ $ sumaDN (U $ U $ D $ D $ D $ D Cero) (U $ U $ D Cero)) + (dNToZ $ sucDN $ D $ U $ U $ U $ D Cero)
--Debe dar: 38
--prueba7 = dNToZ $ zToDNat $ dNToZ $ sumaDN (U $ U $ D $ D $ D $ U $ D Cero) (U $ U $ D $ D $ D Cero)

   --Listas    
--Debe dar: [1,2,3,32,4,6,8,5,0]
--prueba8 = toSet [1,2,3,1,3,3,32,2,4,6,8,5,8,0,1,2,6,0,0,3,2,4,6,2,32]

--Debe dar: 4
--prueba9 = cuantas 1 [1,2,3,1,3,3,32,2,4,6,8,5,8,0,1,2,6,0,0,3,2,4,6,2,1]

--Debe dar: [(1,3),(2,2),(3,3),(32,1),(6,2),(8,2),(5,1),(0,2)]
--prueba10 = frec [1,2,3,1,3,3,32,6,8,5,8,0,1,2,6,0]

--Debe dar: [32,5,7]
--prueba11 = unaVez [1,2,3,1,3,3,32,6,8,5,8,0,1,2,6,0,7]

   --Retos
--Debe dar: "AinacychaninswstliacaiabH"   
--prueba12 = compress1 "And its not a cry you can hear at night, its not somebody who's seen the light, its a cold and its a broken Hallelujah"   

--Debe dar: "acm1pt Sapbeeee!!!"
{- prueba13 = compress2 $ "23"++(replicate 30 'a')++" 100"++(replicate 110 'c')++" 2abm"++" 4mjlo1u"++" 1001"++(replicate 1002 'p')++
               " 0tkajdaad"++" 15sdklf"++" 19"++(replicate 23 'S')++" 8ldjjdlphaph"++" 0pksbhds"++" 1kblgh"++" 2ljekz"++" 3mluekshdgfd"++
               " 4py7jelh"++" 5ñokgoegss"++" 6magdéj!"++" 0!"++" 0!" -}




