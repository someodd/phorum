{- | Customization options and possibly internationalization.

Language/internationalization options begin with `language`.

Style settings begin with `style` (or will soon in the future).

-}

module Config where

maximumPostLength = undefined

-- | The number of minutes a user must wait before posting a new thread.
rateLimitMinutesNewThread :: Int
rateLimitMinutesNewThread = 1

-- | The number of minutes a user must wait before posting a new reply.
rateLimitMinutesNewReply :: Int
rateLimitMinutesNewReply = 1

maximumRepliesPerThread :: Integer
maximumRepliesPerThread = 10

maximumThreads :: Integer
maximumThreads = 10

-- | The max width used for wrapping infolines when converting blocks of texts into info
-- lines. UNIMPLEMENTED
maxWidth :: Integer
maxWidth = 50

-- COMMON/SHARED

-- | The text to view an item as a file.
languageViewAsFile :: String
languageViewAsFile = "View as File"

-- | The text to view an item as a menu.
languageViewAsMenu :: String
languageViewAsMenu = "View as Menu"

-- | The text to display when a user was banned for a post.
languageUserWasBannedForThisPost :: String
languageUserWasBannedForThisPost = "USER WAS BANNED FOR THIS POST"

-- ERRORS ("FRONTEND")

-- | The text to display to a user as an error when they try to post too frequently.
languagePostRateLimitExceeded :: String
languagePostRateLimitExceeded = "Post rate limit exceeded."

-- | The text to display to a user as an error when they try to post and are banned, the
-- text which follows is the actual ban reason/message.
languageYouWereBannedLabel :: String
languageYouWereBannedLabel = "You were banned. Reason: "

-- | Ambiguous error/post insertion failure.
languageFailedToInsertPost :: String
languageFailedToInsertPost = "Failed to insert post."

-- CLI

-- | Text to display when successfully delted a thread by id (the actual post ID will follow).
languageDeletedThreadWithId :: String
languageDeletedThreadWithId = "Deleted thread with ID: "

-- | The text to display upon successfully unbanning an IPv6 address.
languageIpUnbannedSuccess :: String
languageIpUnbannedSuccess = "IP unbanned successfully: "

-- | If trying to ban a post and the post is not found or the IP is missing, this is the error message.
languagePostNotFoundOrIpMissing :: String
languagePostNotFoundOrIpMissing = "Post not found or IP missing."

-- | The success message for adding a given IPv6 address to the ban list. Following is the actual IP address.
languageIpBannedSuccessfully :: String
languageIpBannedSuccessfully = "IP banned successfully: "

-- FILE VIEWS: THREAD VIEW

fileThreadOpBorderHorizontal :: Char
fileThreadOpBorderHorizontal = '='

fileThreadOpBorderVertical :: Char
fileThreadOpBorderVertical = '#'

fileThreadOpBorderCorner :: Char
fileThreadOpBorderCorner = '@'

fileThreadOpMinimumWidth :: Int
fileThreadOpMinimumWidth = 40

fileThreadOpMaximumWidth :: Int
fileThreadOpMaximumWidth = 69

fileThreadOpPadding :: Int
fileThreadOpPadding = 2

fileThreadReplyBorderHorizontal :: Char
fileThreadReplyBorderHorizontal = '-'

fileThreadReplyBorderVertical :: Char
fileThreadReplyBorderVertical = '|'

fileThreadReplyBorderCorner :: Char
fileThreadReplyBorderCorner = '+'

fileThreadReplyMinimumWidth :: Int
fileThreadReplyMinimumWidth = 40

fileThreadReplyMaximumWidth :: Int
fileThreadReplyMaximumWidth = 69

fileThreadReplyPadding :: Int
fileThreadReplyPadding = 2

-- MENU VIEWS: INDEX VIEW

indexReplyLeadingBreak = True

indexReplyTrailingBreak = False

-- | The left side of the heading for the original post in the index view.
indexOpHeadingDecorLeft :: String
indexOpHeadingDecorLeft = "### "

-- | The right side of the heading for the original post in the index view.
indexOpHeadingDecorRight :: String
indexOpHeadingDecorRight = " ###"

-- | The threshold for the number of replies in a thread before they are omitted from the index view.
ommittedRepliesThreshold :: Integer
ommittedRepliesThreshold = 3

languageThreadIndexOpNumberLabel :: String
languageThreadIndexOpNumberLabel = "No. "

languageSingleReplyOmitted :: String
languageSingleReplyOmitted = "1 reply omitted, view thread to see it."

-- | In the index view a threads replies are omitted from the view if there's more than `n`.
languagePluralRepliesOmitted :: String
languagePluralRepliesOmitted = " replies omitted, view thread to see them all."

-- | The text for the index search item used to creating a new thread.
languageCreateThread :: String
languageCreateThread = "Create Thread"

-- | Feel free to use \n! Use ASCII art or whatever! Displayed at the top of the thread index.
indexPreamble :: String
indexPreamble = "Welcome to the Gopher Den!"

-- | The left side of the heading for a reply in a thread view.
indexReplyHeadingDecorLeft :: String
indexReplyHeadingDecorLeft = "* "

-- | The right side of the heading for a reply in a thread view.
indexReplyHeadingDecorRight :: String
indexReplyHeadingDecorRight = ""

-- MENU VIEWS: THREAD VIEW

-- | The text/link/query item for for replying to a thread.
languageThreadReply :: String
languageThreadReply = "Reply to Thread"

-- | The text for the directory item that links back to the index view.
languageReturnToIndex :: String
languageReturnToIndex = "Return to Index"

-- | The left side of the heading for the original post in a thread view.
threadOpHeadingDecorLeft :: String
threadOpHeadingDecorLeft = "### "

-- | The right side of the heading for the original post in a thread view.
threadOpHeadingDecorRight :: String
threadOpHeadingDecorRight = " ###"

-- | The label for the thread number in a thread view.
threadOpThreadNumberLabel :: String
threadOpThreadNumberLabel = "Viewing Thread #"

threadReplyNumberLabel :: String
threadReplyNumberLabel = "Reply #"

threadReplyHeadingDecorLeft :: String
threadReplyHeadingDecorLeft = "---"
