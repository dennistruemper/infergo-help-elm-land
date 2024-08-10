Hi,

so this is something for you to play around with. What I did and why by file:

### Effect.elm

- Added a new effect to toggle the expand state in shared model from a site

### Shared.elm

- Added some example data because I don't want to implement a backend to play with this
- Add handling for ToggleTrackedItem

### Api.TrackedItemList.elm

- add an Id to TrackedItem to make it possible to wire up the toggling part to the correct instance of data. Names could be identical

### Components.TrackedItemDennis.elm

- copied the TrackedItem Compontent and removed everything that has to do with state. Just keeping the view and modified it a little bit. To show how much easier it is if you just use shared oder page state.

### Home\_.elm

- wire up shared for view
- use TrackedItemDennis to show items in shared model

### Shared.Model.elm

- add id to TrackedItem type, same reason as before. Type is duplicated. Maybe you don't want that and reuse the type alias in API

### Shared.Msg.elm

- add new message ToggleTrackedItem to toggle the expanded state of an TrackedItem by Id.

## Overall stuff I noticed:

- Try to learn about making impossible states impossible and use it : https://www.youtube.com/watch?v=IcgmSRJHu_8
- Try to learn about single source of truth and use it: (think it was in here. richard has too many good talks) https://www.youtube.com/watch?v=x1FU3e0sT1I
  - this will avoid duplicated data (if you need it, its fine. But most of the time its just a source for errors you don't want)
