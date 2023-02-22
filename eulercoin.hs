
ec :: Int -> Integer
ec = (map ecH [0 ..] !!)
  where ecH n = (1504170715041707*n) `mod` 4503599627370517
--[8912517754604,2044785486369,1311409677241,578033868113,422691927098,267349986083,112008045068,68674149121,25340253174,7346610401,4046188430,745766459,428410324,111054189,15806432

ecR n = ((-4503599627370517)*n) `mod` 1504170715041707
ecAddR s = (s - 4503599627370517) `mod` 1504170715041707

ecsRHelp :: Int -> Int -> [Int]
ecsRHelp l c
  | c == 4503599627370517 = []
  | c < l = c:ecsRHelp c (ecAddR c)
  | otherwise = ecsRHelp l (ecAddR c)
ecsR :: [Int]
ecsR = ecsRHelp (ecAddR 0) (ecAddR (ecAddR 0))

ecAdd start = (1504170715041707 + start) `mod` 4503599627370517
ecAdd' s 
  | (1504170715041707 + s) < 4503599627370517 = 1504170715041707 + s
  | otherwise = (1504170715041707 + s) - 4503599627370517 

ecsHelp :: Int -> Int -> [Int]
ecsHelp l c
  | c == 1504170715041707 = []
  | c < l = c:ecsHelp c (ecAdd' c)
  | otherwise = ecsHelp l (ecAdd' c)

ecs :: [Int]
ecs = ecsHelp (ecAdd' 0) (ecAdd' (ecAdd' 0))

