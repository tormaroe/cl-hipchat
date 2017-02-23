**cl-hipchat** is a HipChat API wrapper client library for Common Lisp.

State: Usable but not complete. Breaking changes may occur.

## Initialization

You will not found cl-hipchat in QuickLisp yet, but it you clone this repo into your QuickLisp local-projects folder it does not matter. Load it like this:

```
(ql:quickload :cl-hipchat)
```

Before you can make any API requests you need to set the access token. You can find information about [HipChat's access tokens here](https://developer.atlassian.com/hipchat/guide/hipchat-rest-api/api-access-tokens), but the simplest way to get started is to use your [personal access token](https://www.hipchat.com/account/api).

Once you have obtained a token, you may set it globaly like this:

```
(setf hipchat.config:*AUTH-TOKEN* "your-token-value")
```

.. or use `let` to create a dynamic binding scope.

```
(let ((hipchat.config:*AUTH-TOKEN* "your-token-value"))
  ;; Do your API calls here...
  )
```

## Example

In this example we retrieve the list of all available rooms and echo the most recent message in one of them.

```
(let ((hipchat.config:*AUTH-TOKEN* "HUIr76trDERT6dhwnweifu78GHFhmmi"))
  (let* ((lucky-room (car (hipchat:get-all-rooms)))
         (room-id (hipchat:room-id lucky-room))
         (latest-message (car (hipchat:recent-room-history room-id :max 1))))
    (hipchat:send-notification room-id 
                               (hipchat:message-text latest-message) 
                               :from "Echo bot" 
                               :color :random)))
``` 
Notice that `hipchat` exists as a nickname (alias) for `cl-hipchat`.

## Return value conventions

An API function call is a success as long as the call returns a value other than `nil`.

Data will be returned as association lists.

Some exceptional situations may signal an error condition.

## API

### Room functions

**get-room (room-id-or-name)**<br>
Gets the details for a room. <br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/get_room)

**get-all-rooms (&key (start 0) (max 100) (include-private t) include-archived)**<br>
Get all rooms, optionally including private and archived rooms. Results are paged, and `max` returned rooms per request should be between 0 and 1000 (defaults to 100). <br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/get_all_rooms)

**create-room (name &key guest-access owner-user-id (privacy :public) (topic ""))**<br>
Creates a new room. Only the `name` parameter (length between 1 and 50) is required. <br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/create_room)

**delete-room (room-id-or-name)**<br>
Delete a room. <br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/delete_room)

**set-topic (room-id-or-name topic)**<br>
Set a room's topic. <br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/set_topic)

### User functions

**get-user (name)**

**get-all-users (&key (start 0) (max 100) include-guests include-deleted)**

### Send functions

**send-notification (room-id-or-name message &key (from "") (color :yellow) notify (message-format :html))**<br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/send_room_notification)

**send-message (room-id-or-name message)**<br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/send_message)

**send-private-message (user message &key notify (message-format :text))**<br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/private_message_user)

**share-file-with-room (room filepath &optional message)**<br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/share_file_with_room)

**share-link-with-room (room-id-or-name link &optional (message ""))**<br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/share_link_with_room)

### Read functions

**room-history (room &key (date "recent") (timezone "UTC") (start 0) (max 100) (reverse t))**<br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/view_room_history)

**recent-room-history (room &key not-before (timezone "UTC") (max 100) (include-deleted t))**<br>
[HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/view_recent_room_history)