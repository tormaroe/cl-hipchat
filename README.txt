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
(setf cl-hipchat.config:*AUTH-TOKEN* "your-token-value")
```

.. or use `let` to create a dynamic binding scope.

```
(let ((cl-hipchat.config:*AUTH-TOKEN* "your-token-value"))
  ;; Do your API calls here...
  )
```

## Return value conventions

An API function call is a success as long as the call returns a value other than `nil`.

Data will be returned as association lists.

Some exceptional situations may signal an error condition.

## API

### Room functions

**get-room (room-id-or-name)**<br>
Gets the details for a room. [HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/get_room)

**get-all-rooms (&key (start 0) (max 100) (include-private t) include-archived)**<br>
Get all rooms, optionally including private and archived rooms. Results are paged, and `max` returned rooms per request should be between 0 and 1000 (defaults to 100). [HipChat API detials..](https://www.hipchat.com/docs/apiv2/method/get_all_rooms)

*Work in progress...*