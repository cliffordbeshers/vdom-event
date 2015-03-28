> {-# LANGUAGE CPP #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Radio where

> import Control.Monad.Trans
> import Data.Monoid ((<>))
> import Data.Default (def)
> import Data.IORef
> import Outline
> import Data.Proxy
> -- import Data.Tagged
> import Data.Text as Text (Text, pack)
> import Lucid
> import Bootstrap.Utils (classes_, textshow)
#if CLIENT
> import JavaScript.JQuery as JQuery (Event,JQuery, appendJQuery, click, select, setHtml, setText, getHtml, addClass, removeClass)
#endif


> default (Text)

> type URL = Text

> imageURLs =
>   [ "https://fbcdn-sphotos-e-a.akamaihd.net/hphotos-ak-xaf1/t31.0-8/11082361_10204865703177218_7142048938861798417_o.jpg"
>   , "https://scontent-ord.xx.fbcdn.net/hphotos-xpa1/v/t1.0-9/s720x720/10676351_10204875971793927_291876182821710665_n.jpg?oh=77cf176c31c46ac933272de0480dcfe1&oe=55818649"
>   , "https://fbcdn-sphotos-c-a.akamaihd.net/hphotos-ak-xpf1/t31.0-8/10914721_10204856412024945_2607390891971097151_o.jpg"
>   , "https://scontent-ord.xx.fbcdn.net/hphotos-xpf1/v/t1.0-9/10988282_10204849112762468_7979569982779097370_n.jpg?oh=712dc71ac4a1a5c7a6d452ec57d6799f&oe=5583185E"
>   , "https://scontent-ord.xx.fbcdn.net/hphotos-xfp1/t31.0-8/11073500_10204842190469415_875697331045021877_o.jpg"
>   , "https://fbcdn-sphotos-g-a.akamaihd.net/hphotos-ak-xpa1/v/t1.0-9/10710956_10204829354548525_3840378263239480270_n.jpg?oh=a6f915cf68b916c6c84c6e8baeecb028&oe=55B52B34&__gda__=1434306594_f075e1695ab5a3a56ff0a6d7921b6254"
>   , "https://scontent-ord.xx.fbcdn.net/hphotos-xpf1/v/t1.0-9/10983182_10204802735643069_2375287011727111812_n.jpg?oh=c57d524c6be33146eb175548a7f46200&oe=5573FFE1"
>   , "https://fbcdn-sphotos-d-a.akamaihd.net/hphotos-ak-prn2/v/t1.0-9/11056894_10204795437740626_1302633900976191151_n.jpg?oh=da776a52e13eba9c1426b03708f1fd15&oe=557A2D8C&__gda__=1437981455_ed50f8cf7a166212a9e087b81fb3cd5c"
>   ]

> radioExample1 :: Monad m => OutlineT m () -- [(Ident,Outline ())]
> radioExample1 = do
>   bs <- identify' $ map (button . img 125) imageURLs
>   vbox $ sequence_ $ map snd bs
>   -- mapM_ (\i -> liftIO (select (byId i) >>= click (mkCircle i) def)) $ map fst bs
>   return () -- liftIO $ putStrLn "hello"  
>     where img :: Monad m => Int -> Text -> OutlineT m ()
>           img sq u = img_ [src_ u, width_ (textshow sq), height_ (textshow sq)]
>           mkRounded, mkCircle :: Text -> Event -> IO ()
>           mkRounded ident _ = select (byId ident) >>= addClass "img-rounded" >>= removeClass "img-circle" >> return ()
>           mkCircle ident _ = select (byId ident) >>= addClass "img-circle" >>= removeClass "img-rounded" >> return ()
>           button :: Monad m => HtmlT m () -> HtmlT m ()
>           button = button_ [classes_ ["btn","btn-default"], type_ "submit"]
> 

> radioExample :: Monad m => OutlineT m ()
> radioExample = div_ $ do
>   radioExample2
>   fakeRadioState (imageButtonMarkup 125) (FakeRadioState imageURLs Nothing)
>   fakeRadioState (imageButtonMarkup 125) (FakeRadioState imageURLs (Just 6))

> radioExample2 :: Monad m => OutlineT m ()
> radioExample2 = radioState (imageButtonMarkup 125) (RadioState imageURLs 4)

> imageButtonMarkup :: Monad m => Int -> Bool -> URL -> OutlineT m ()
> imageButtonMarkup sq selected url = img sq url
>   where
>     img sq u = img_ [src_ u, width_ (textshow sq), height_ (textshow sq), classes_ cs]
>     cs = ["img-rounded"] <> (if selected then ["img-circle"] else [])

> data RadioState a = RadioState { rbs :: [a], selected :: Int }

> zipCollected :: Int -> [a] -> [(Bool,a)]
> zipCollected i xs = zip (map (== i) [0..]) xs

> radioState :: Monad m => (Bool -> a -> OutlineT m ()) -> RadioState a -> OutlineT m ()
> radioState innerMarkup rs = do
>   bs <- identify' $  map (unwrap . wrap button . wrap innerMarkup) (zipCollected (selected rs) $ rbs rs)
>   vbox $ sequence_ $ map snd bs
>   -- mapM_ (\i -> liftIO (select (byId i) >>= click (mkCircle i) def)) $ map fst bs
>   return () -- liftIO $ putStrLn "hello"  
>     where button :: Monad m => Bool -> HtmlT m () -> HtmlT m ()
>           button sel = button_ [classes_ (["btn","btn-default"] <> active sel), type_ "button"]
>           active :: Bool -> [Text]
>           active b = if b then [ "active" ] else []
>           wrap :: (a -> b -> c) -> (a,b) -> (a,c)
>           wrap f (a,b) = (a, f a b)
>           unwrap = snd


> data FakeRadioState a = FakeRadioState { frbs :: [a], fakeSelected :: Maybe Int }

> zipFakeCollected :: Maybe Int -> [a] -> [(Bool,a)]
> zipFakeCollected Nothing xs = zip (repeat False) xs
> zipFakeCollected (Just i) xs = zip (map (== i) [0..]) xs

> fakeRadioState :: Monad m => (Bool -> a -> OutlineT m ()) -> FakeRadioState a -> OutlineT m ()
> fakeRadioState innerMarkup rs = do
>   bs <- identify' $  map (unwrap . wrap button . wrap innerMarkup) (zipFakeCollected (fakeSelected rs) $ frbs rs)
>   vbox $ sequence_ $ map snd bs
>   -- mapM_ (\i -> liftIO (select (byId i) >>= click (mkCircle i) def)) $ map fst bs
>   return () -- liftIO $ putStrLn "hello"  
>     where button :: Monad m => Bool -> HtmlT m () -> HtmlT m ()
>           button sel = button_ [classes_ (["btn","btn-default"] <> active sel), type_ "button"]
>           active :: Bool -> [Text]
>           active b = if b then [ "active" ] else []
>           wrap :: (a -> b -> c) -> (a,b) -> (a,c)
>           wrap f (a,b) = (a, f a b)
>           unwrap = snd


-- > renderRadio :: StateRadio -> Html
-- > renderRadio state = ul_ $ mapM_ li_ els
-- >   where irs = zip [0..] (rs state)
-- >         els = map f irs
-- >         f (i,r) = do el <- if i == (selected state) then unselected r else selected r
-- >                      click (\ev -> (\s -> s { selected = i })) el
-- >         unselected :: String -> Html
-- >         unselected s = button_ [classes ["img-rounded"]
-- >         button :: Monad m => HtmlT m () -> HtmlT m ()
-- >         button = button_ [classes_ ["btn","btn-default"], type_ "submit"]
-- >           mkRounded ident _ = select (byId ident) >>= addClass  >>= removeClass "img-circle" >> return ()
-- >           mkCircle ident _ = select (byId ident) >>= addClass "img-circle" >>= removeClass "img-rounded" >> return ()


> vbox :: Monad m => HtmlT m () -> HtmlT m ()
> vbox = div_ -- TODO Bootstrap equiv of vbox and hbox


-- > data StateRadio = StateRadio { rs :: [String], selected :: Int }
-- > data RadioStyle = RadioStyle {  }




-- > radioButton :: [a] -> Int -> Outline a
-- > radioButton xs i = do
-- >   xs' < identify xs
-- >   xref <- liftIO $ newIORef (fst (xs !! i))
-- >   mapM (radioButton' . tagURL) xs
-- >   mapM (\x -> click (\_ -> writeIORef xref x)) xs
-- >     where tagURL (i,a) = (i, a, tagWith (Proxy a) "url")

-- > radioButton :: Monad m => [(Ident,)] -> (Ident,a) -> OutlineT m ()
-- > radioButton xs x = do
-- >   xref <- liftIO $ newIORef x
-- >   mapM (radioButton' . tagURL) xs
-- >   mapM (\x -> click (\_ -> writeIORef xref x)) xs
-- >     where tagURL (i,a) = (i, a, tagWith (Proxy a) "url")

-- > radioButton' :: Monad m => (Ident, a, Tagged a URL) -> OutlineT m ()
-- > radioButton' = plug . markup . untag
-- >   where plug = button_ [classes_ ["btn","btn-default", "btn-block"], type_ "submit"]
-- >         markup u = img_ [src_ u, width_ "100", height_ "100"]
