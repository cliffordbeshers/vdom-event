10:51:20 PM) thetallguy: I believe I've found the pieces to do it, but it looks a little arduous, and I wondered if I'd missed a trick
(10:53:53 PM) luite: thetallguy: if you have an aeson FromJSON instance you can combine that with the FromJSRef instance for an aeson Value to build a FromJSRef instance for it
(10:54:31 PM) thetallguy: I don't see any deriving of the the To/FromJSon
(10:57:29 PM) luite: thetallguy: data MyData = MyData { .... } 
(10:57:38 PM) luite: deriveJSON defaultOptions ''MyData
(10:57:54 PM) thetallguy: oh.  I searched for deriving
(10:58:15 PM) thetallguy: Time to sleep, I'lldo this in the morning when I have eyes and brain  Thanks.
(10:58:24 PM) luite: that's the TH method. without TH is also possible, through generics: data MyData = MyData { .... } deriving Generic
(10:58:37 PM) luite: and then: instance FromJSON MyData
(10:59:01 PM) thetallguy: Gotcha.
(10:59:03 PM) thetallguy: thanks
(10:59:04 PM) luite: an empty instance will default to one based on the GHC.Generics one
(10:59:23 PM) luite: template haskell instances tend to be more efficient though, and use less code
(10:59:25 PM) thetallguy: A JSRef is an actual javascript structure right, phantom typed to what it encodes?
(10:59:31 PM) luite: yeah
(11:00:34 PM) luite: best performance is probably to parse the JSON in js, and then the Value FromJSRef
(11:01:06 PM) luite: that still creates the intermediate aeson Value thing, i'd like to add an aeson compatible deriver that directly pulls things out of the JSRef
(11:51:48 PM) thetallguy: That would be good, but efficiency is not my issue right now

(11:58:13 PM) luite: thetallguy: if you need even more efficiency you could use Binary instances, especially if you have a binary communications channel (latest spec websockets for example)
(11:58:31 PM) luite: deriving those works the same way
