
ec :: Int -> Integer
ec = (map ecH [0 ..] !!)
  where ecH n = (1504170715041707*n) `mod` 4503599627370517
--[8912517754604,2044785486369,1311409677241,578033868113,422691927098,267349986083,112008045068,68674149121,25340253174,7346610401,4046188430,745766459,428410324,111054189,15806432

ec' n start = (1504170715041707 * n + start) `mod` 4503599627370517

ecsHelp l c i
  | c < l = c:ecsHelp c (ec' 1 c) 1
  | c < 1000000 = []
  | otherwise = ecsHelp l (ec' (i+1) l) (i+1)

ecs = ecsHelp (ec 1) (ec 2) 1

