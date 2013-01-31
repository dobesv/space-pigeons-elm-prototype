import Automaton (init')
import Maybe
import Text
import Graphics


fromMaybe default option = maybe default id option
mapMaybe f xs = justs (map f xs)

gameWidth = 1024
gameHeight = 768
xga = { width=1024, height=768 }
ico = { width=100, height=100 }

indexBy f lst = Dict.fromList (map (\v -> let k = f v in (k,v)) lst)
indexById lst = indexBy (show . .id) lst


groupBy f lst =
    let addToGroup v dict =
           let k = f v
           in Dict.insert k v:(Dict.findWithDefault [] k dict)
    in  List.foldr addToGroup Dict.empty lst
--
--groupByRoom lst = groupBy .room lst

f >>> g = g . f
x |> g = g x
half x = x `div` 2

data ItemID = Screwdriver
            | Superphone
            | Knife

data RoomID = Office
            | Corridor
            | Infirmary
            | EscapePod

data EventID = BoughtCake


data Cond = EventOccurred EventID
          | Have ItemID
          | InRoom RoomID
          | Always
          | CondNot Cond
          | CondAnd Cond Cond
          | CondOr Cond Cond


-- Screwdriver
itemList = [
    { id=Screwdriver, icon="screwdriver.png" },
    { id=Superphone,  icon="superphone.png" },
    { id=Knife,       icon="knife.png"}
 ]
items = indexById itemList
badItemId = { id=Screwdriver, icon="XXX.png" }

rooms =
    let roomList = [
            { id=Corridor,  bg = {xga|image="Corridor.jpg"} },
            { id=Infirmary, bg = {xga|image="Infirmary.jpg"} },
            { id=EscapePod, bg = {xga|image="Escape Pod.jpg"} },
            { id=Office,    bg = {xga|image="Office.jpg"} }
        ]

        -- Doors
        doors = [
            { room=Office,    left=473, top= 99, width=248, height=407, targetRoom=Corridor  }, -- Office Exit
            { room=Corridor,  left=140, top=320, width= 96, height=120, targetRoom=Office    }, -- Office Entrance
            { room=Corridor,  left=770, top=320, width= 96, height=120, targetRoom=Infirmary }, -- Infirmary Entrance
            { room=Infirmary, left=665, top=232, width=313, height=395, targetRoom=Corridor  }, -- Infirmary Exit
            { room=Corridor,  left=305, top=469, width=118, height= 26, targetRoom=EscapePod }, -- Escape Pod Entrance
            { room=EscapePod, left=294, top=139, width=199, height= 91, targetRoom=Corridor  }  -- Infirmary Exit
         ]

        -- Props
        props = [
            {room=Office,   left=117, top=249, width= 59, height= 93, image=Nothing, description="Some kind of display terminal.  Probably displays very useful information; if I only knew how to turn it on!"},
            {room=Office,   left=183, top=316, width=106, height=213, image=Nothing, description="My predecessor."},
            {room=Office,   left=738, top=290, width=133, height=390, image=Nothing, description="Could they be part of my job here?"},
            {room=Office,   left=213, top=466, width=100, height=100, image=Just "cake.png", description="A cake.  I can't eat it."},
            {room=Corridor, left=514, top=469, width=111, height= 25, image=Nothing, description="Looks like the escape pod has already launched!"}
         ]

        -- Pickups
        pickups = [
            {room=Office,   left=190, top=291, width=100, height=100, label="screw driver", item=Screwdriver, image="ScrewDriver_1_screen.png"},
            {room=Office,   left=169, top=379, width=100, height=100, label="phone", item=Superphone,  image="phone_pocket.png"},
            {room=Office,   left=202, top=465, width=133, height=111, label="cake with knife", item=Knife, image="cake_knife.png"}
         ]

        aug r =
            let flt r lst = List.filter (.room >>> ((==) r.id)) lst
                r1 = {r | doors = flt r doors}
                r2 = {r1 | props = flt r props}
                r3 = {r2 | pickups = flt r pickups}
            in r3
    in indexById (map aug roomList)

badRoomId = {id=Office, bg = {xga|image="Missing.jpg"}, doors=[], props=[], pickups=[]}

-- Game state
data Activity = Idle
              | ShowDescription {text::String, endTime::Int}
              | FadeOutTo {next::Activity, startTime::Int, endTime::Int}
              | FadeInTo {next::Activity, startTime::Int, endTime::Int}
              | ChangeToRoom RoomID
              | PickUpItem ItemID
              | UseItem ItemID

startState = {
    room = Office,
    inventory = Dict.empty,
    showingInventory = True,
    activity = Idle,
    dialog = Nothing,
    fade = 0, -- 0 to 100 level of "fadedness"
    lastClick = {}
 }

inRect x y rect =
    let dx = x - rect.left
        dy = y - rect.top
    in dx >= 0 && dx < rect.width &&
       dy >= 0 && dy < rect.height
headMaybe lst = case lst of
    []  -> Nothing
    x:_ -> Just x

find p lst = case lst of
    [] -> Nothing
    x:xs -> if p x then Just x else find p xs

findRoom id = Dict.findWithDefault badRoomId (show id) rooms
findItem id = Dict.findWithDefault badItemId (show id) items

tryInTurn defaultValue flist =
    case flist of
        [] -> defaultValue
        f:fs -> case f () of
            Nothing -> tryInTurn defaultValue fs
            Just v -> v
maybeMap f x =
    case x of
        Just v -> Just (f v)
        Nothing -> Nothing


inventoryWidth = 809
inventoryHeight = 106
inventoryArea = {
    width = inventoryWidth,
    height = inventoryHeight,
    left = ((gameWidth - inventoryWidth) `div` 2),
    top = gameHeight - inventoryHeight,
    item = {
        top = 3, -- relative to inventory area top
        left = 3, -- relative to inventory area left
        width = 100,
        height = 100
    },
    bg = "images/ui/inventorybg.png"
 }

dialogTextColor = Color.rgb 220 220 220

-- Defaults for offset
noOffset = {left=0,top=0}

-- Offset an element by the given x and y (in pixels) in its parent container
offsetBy {left,top} elt =
    let width = left + (widthOf elt)
        height = top + (heightOf elt)
        location = topLeftAt (absolute left) (absolute top)
     in container width height location elt

-- Defaults for padding
noPadding = {left=0,top=0,right=0,bottom=0}

-- Put an element into a slightly larger container, adding some padding around it
paddedBy {left,top,right,bottom} elt =
    let location = topLeftAt (absolute left) (absolute top)
        width = left + (widthOf elt) + right
        height = top + (heightOf elt) + bottom
    in container width height location elt

-- Find the nth element of a list, or Nothing if the list is too short
nthMaybe n lst =
    case lst of
        [] -> Nothing
        x:xs -> if n == 0 then Just x else nthMaybe (n-1) xs

-- Position the object in the middle of a box bw by bh
centerIn bw bh elt =
    let ew = widthOf elt
        pl = (bw - ew) `div` 2
        pr = bw - ew - pl
        eh = heightOf elt
        pt = (bh - eh) `div` 2
        pb = bh - eh - pt
    in paddedBy {left = pl, top = pt, right=pr, bottom=pb } elt

fadeTime = 500
descriptionTime = 5000

updateGame input oldState =
    let {t,mouse} = input
        clicked = mouse.clicked
        underMouse obj = inRect mouse.x mouse.y obj
        oldRoom = findRoom oldState.room
        pickupsNotPickedUp = filter (\p -> (Dict.lookup (show p.item) oldState.inventory) == Nothing) oldRoom.pickups
        heldItems = Dict.values oldState.inventory
        findClickedOn objs = if clicked then find underMouse objs else Nothing
        newActivity =
            let oldActivity = oldState.activity
                fadeOutTo next = FadeOutTo {next=next, startTime=t, endTime=t+fadeTime}
                fadeInTo next = FadeInTo {next=next, startTime=t, endTime=t+fadeTime}
                checkFade () =
                    case oldState.activity of
                        FadeOutTo x -> Just (if x.endTime < t then x.next else oldState.activity)
                        FadeInTo x -> Just (if x.endTime < t then x.next else oldState.activity)
                        otherwise -> Nothing
                checkEnteredRoom () =
                    case oldActivity of
                        ChangeToRoom _ -> Just (fadeInTo Idle)
                        otherwise -> Nothing
                showDescription text = ShowDescription {text=text, endTime=t+descriptionTime}
                checkUseItem () = Nothing
                checkMiniGame () =
                    case oldActivity of
                        UseItem Superphone -> Nothing
                        otherwise -> Nothing
                checkDoor () = maybeMap (.targetRoom >>> (fadeOutTo . ChangeToRoom)) (findClickedOn oldRoom.doors)
                checkPickup () = maybeMap (.item >>> PickUpItem) (findClickedOn pickupsNotPickedUp)
                checkProp () = maybeMap (.description >>> showDescription) (findClickedOn oldRoom.props)
                checkInventory () =
                    if clicked then
                        if underMouse inventoryArea then
                            let firstItemLeft = mouse.x - inventoryArea.left - inventoryArea.item.left
                                invIndex = firstItemLeft `div` inventoryArea.item.width
                            in (nthMaybe invIndex heldItems) |> maybeMap UseItem
                        else Nothing -- TODO Support using an item on something ...
                    else case oldActivity of
                        UseItem n -> Just oldActivity
                        otherwise -> Nothing

                checkDescription () =
                    case oldActivity of
                        ShowDescription x -> if clicked || (x.endTime < t) then Nothing else Just oldActivity
                        otherwise -> Nothing
            in tryInTurn Idle [
                checkFade,
                checkMiniGame,
                checkEnteredRoom,
                checkUseItem,
                checkPickup,
                checkProp,
                checkDoor,
                checkInventory,
                checkDescription
            ]
        newState = {
            room=case newActivity of
                     ChangeToRoom id -> id
                     otherwise -> oldState.room,
            inventory=case newActivity of
                          PickUpItem id -> Dict.insert (show id) id oldState.inventory
                          otherwise -> oldState.inventory,
            activity=newActivity,
            lastClick = if clicked then {mouse - clicked} else oldState.lastClick
        }
        newRoom = if newState.room == oldState.room then oldRoom else findRoom newState.room
        bg = newRoom.bg
        bgImage = image bg.width bg.height ("images/bg/" ++ bg.image)
        debugView =
            let debugTextView = show >>> toText >>> (Text.color green) >>> Text.monospace >>> Graphics.text
                views = [input,oldState,newState]
            in flow down (map debugTextView views)
        dialogView =
            let text = case newActivity of
                            ShowDescription d -> let {text} = d in text
                            otherwise -> ""
            in text |> (toText
                    >>> (Text.color dialogTextColor)
                    >>> (Text.height 1.5)
                    >>> (Text.typeface "'Trebuchet MS', Helvetica, sans-serif")
                    >>> centeredText
                    >>> (Graphics.width gameWidth)
                    >>> (Graphics.color (Color.rgba 0 0 0 0.5))
                    >>> (container gameWidth (half gameHeight) middle))
        tipView =
            let checkPickup () = maybeMap (.label >>> ((++) "Pick up ")) (find underMouse pickupsNotPickedUp)
                tipText = tryInTurn "" [
                    checkPickup
                ]
            in tipText |> (toText
               >>> (Text.color dialogTextColor)
               >>> (Text.height 1)
               >>> (Text.typeface "'Trebuchet MS', Helvetica, sans-serif")
               >>> centeredText
               >>> (Graphics.width gameWidth)
               >>> (Graphics.color (Color.rgba 0 0 0 0.5))
               >>> (container gameWidth (half gameHeight) middle))

        visibleProps = mapMaybe (\ p -> maybeMap (\img -> {p|image<-img}) p.image) oldRoom.props
        propView p = offsetBy p (image p.width p.height ("images/props/"++p.image))
        propViews = map propView visibleProps
        pickupViews = map propView pickupsNotPickedUp
        inventoryView =
            if newState.inventory == Dict.empty then plainText "" else
                let itemImage item = image inventoryArea.item.width inventoryArea.item.height ("images/icon/"++item.icon)
                    itemIcons = flow right (List.map (findItem >>> itemImage) heldItems)
                    itemIconArea = offsetBy inventoryArea.item itemIcons
                    itemBg = image inventoryArea.width inventoryArea.height inventoryArea.bg
                in layers [itemBg, itemIconArea]
        applyFade elt =
            let fadeAmount {next, startTime, endTime} = (t - startTime) / (endTime - startTime)
                alpha = case newActivity of
                            FadeOutTo x -> 1.0 - (fadeAmount x)
                            FadeInTo x  -> fadeAmount x
                            ChangeToRoom _ -> 0.0 -- While changing rooms
                            otherwise -> 1.0
            in if alpha < 1.0 then opacity alpha elt else elt
        bottomUIView = container gameWidth gameHeight midBottom
            (flow down [tipView, offsetBy inventoryArea inventoryView])

        miniGameViews = case newActivity of
            UseItem Superphone -> [centerIn gameWidth gameHeight (image 454 707 "images/ui/superphone.png")]
            otherwise -> []

        stage = layers ([bgImage] ++ propViews ++ pickupViews ++ [bottomUIView, dialogView] ++ miniGameViews)
        view = Graphics.color black (flow down [applyFade stage, debugView])
    in (view,newState)

mkinput t clicked (mouseX,mouseY) = let r = {t=t, mouse={clicked=clicked, x=mouseX, y=mouseY}} in r
input = lift3 mkinput (Time.every 0.1) Mouse.isClicked Mouse.position
gameAutomaton = init' startState updateGame
main = Automaton.run gameAutomaton input
