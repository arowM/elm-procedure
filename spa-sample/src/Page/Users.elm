module Page.Users exposing
    ( Command(..)
    , Event
    , Memory
    , Procedures
    , init
    , procedures
    , runCommand
    , view
    )

import App.Session exposing (Session)
import App.Route as Route
import Browser.Navigation exposing (Key)
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer as Observer exposing (Observer)
import Procedure.ObserverId exposing (ObserverId)
import Procedure.VPack as VPack exposing (VPack)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { pageView : PageView
    , session : Session
    , toast : Toast.Memory
    }


{-| -}
init : Session -> Memory
init session =
    { pageView = LoadingPageView
    , session = session
    , toast = Toast.init
    }


type PageView
    = LoadingPageView
    | UsersPageView ( ObserverId, UsersPageMemory )


{-| -}
type Event
    = ToastEvent Toast.Event
    | UsersPageEvent UsersPageEvent



-- View


{-| -}
view :
    VPack e Memory Event
    -> Html (Msg e)
view page =
    let
        param =
            VPack.memory page
    in
    Html.div
        [ localClass "page"
        ]
        [ case param.pageView of
            LoadingPageView ->
                loadingPageView

            UsersPageView usersPage ->
                VPack.child
                    page
                    UsersPageEvent
                    (\_ -> usersPageView param.session)
                    usersPage
        , VPack.child
            page
            ToastEvent
            (\_ -> Toast.view)
            (VPack.observerId page, param.toast)
        ]




-- -- LoadingPage


loadingPageView : Html msg
loadingPageView =
    Html.div
        [ localClass "loading"
        ]
        [ Html.text "Loading..."
        ]



-- -- LoginFormPage


type alias UsersPageMemory =
    { users : List (ObserverId, UserFieldMemory)
    , newUserForm : NewUserFormMemory
    }


initUsersPage : UsersPageMemory
initUsersPage =
    { users = []
    , newUserForm = initNewUserForm
    }


type UsersPageEvent
    = UserFieldEvent UserFieldEvent
    | NewUserFormEvent NewUserFormEvent


usersPageView :
    Session
    -> VPack e UsersPageMemory UsersPageEvent
    -> Html (Msg e)
usersPageView session usersPage =
    let
        param =
            VPack.memory usersPage
    in
    Html.div
        [ localClass "users"
        ]
        [ case param.editAccountForm of
            Nothing ->
                Html.div
                    [ localClass "users_account"
                    ]
                    [ Html.text <| "Hello, " ++ session.id
                    , Html.node "button"
                        [ localClass "users_account_editButton"
                        , Events.onClick
                            (VPack.issue usersPage <| ClickShowEditAccountForm)
                        ]
                        [ Html.text "🖉"
                        ]
                    ]
            Just editAccountFormCore ->
                VPack.child
                    usersPage
                    EditAccountFormEvent
                    (\_ -> editAccountFormView)
                    editAccountFormCore
        , Html.div
            [ localClass "users_links"
            ]
            [ Html.a
                [ localClass "users_links_linkButton-users"
                , Mixin.attribute "href" <| Route.toPath Route.Users
                ]
                [ Html.text "Users"
                ]
            ]
        ]


-- -- NewUserForm

type alias NewUserFormMemory =
    { form : NewUser.Form
    , isBusy : Bool
    , showError : Bool
    }


initNewUserForm : NewUserFormMemory
initNewUserForm =
    { form = NewUser.init
    , isBusy = False
    , showError = False
    }


type NewUserFormEvent
    = ChangeNewUserName String
    | ClickSubmitNewUser
    | ReceiveCreateNewUser (Result Http.Error NewUser.Response)


newUserFormView :
    VPack e NewUserFormMemory NewUserFormEvent
    -> Html (Msg e)
newUserFormView newUserForm =
    let
        param = VPack.memory newUserForm

        errors = NewUser.toFormErrors param.form
    in
    Html.div
        [ localClass "newUserForm"
        , Mixin.boolAttribute "aria-invalid"
            (param.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "newUserForm_nameLabel"
            ]
            [ Html.text "New User:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" param.form.id
                , Mixin.boolAttribute "disabled" param.isBusy
                , Events.onChange
                    (VPack.issue newUserForm << ChangeNewUserName)
                ]
                []
            ]
        , Html.node "button"
            [ localClass "newUserForm_button-submit"
            , Events.onClick
                (VPack.issue newUserForm ClickSubmitNewUser)
            , Mixin.boolAttribute "disabled" param.isBusy
            ]
            [ Html.text "Add"
            ]
        ]


-- -- UserField

type alias UserFieldMemory =
    { editUserForm : Maybe (ObserverId, EditUserFormMemory)
    , user : User
    }


initUserFieldMemory : User -> UserFieldMemory
initUserFieldMemory user =
    { editUserForm = Nothing
    , user = user
    }


type UserFieldEvent
    = EditUserFormEvent EditUserFormEvent
    | ClickEditUser
    | ClickRemoveUser


userFieldView :
    VPack e UserFieldMemory UserFieldEvent
    -> Html (Msg e)
userFieldView userField =
    let
        param = VPack.memory userField
    in
    Html.div
        [ localClass "userField"
        ]
        [ case editUserForm of
            Nothing ->
                Html.div
                    [ localClass "userField_view"
                    ]
                    [ Html.div
                        [ localClass "userField_view_id"
                        ]
                        [ Html.text "ID: " ++ param.user.id
                        ]
                    , Html.div
                        [ localClass "userField_view_name"
                        ]
                        [ Html.text param.user.name
                        ]
                    , Html.node "button"
                        [ localClass "usersField_view_editButton"
                        , Events.onClick
                            (VPack.issue usersPage <| ClickEditUser)
                        ]
                        [ Html.text "🖉"
                        ]
                    , Html.node "button"
                        [ localClass "usersField_view_removeButton"
                        , Events.onClick
                            (VPack.issue usersPage <| ClickRemoveUser)
                        ]
                        [ Html.text "🗙"
                        ]
                    ]
                Just editUserFormCore ->
                    VPack.child
                        userField
                        EditUserFormEvent
                        (\_ -> editUserFormView)
                        editUserFormCore
        ]


-- Procedures


{-| -}
type Command e
    = ToastCommand (Toast.Command e)
    | EditAccountCommand (EditAccount.Command e)


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand

        EditAccountCommand editAccountCommand ->
            EditAccount.runCommand editAccountCommand


{-| -}
type alias Procedures m e =
    List (Procedure (Command e) m e)



-- -- Initialization


{-| -}
procedures :
    Key
    -> Observer m e Memory Event
    -> Procedures m e
procedures key page =
    let
        toast =
            Observer.inherit
                { get = .toast
                , set = \m2 m1 -> { m1 | toast = m2 }
                , unwrap =
                    \e1 ->
                        case e1 of
                            ToastEvent e2 ->
                                Just e2

                            _ ->
                                Nothing
                , wrap = ToastEvent
                }
                page
    in
    [ Procedure.observe initUsersPage <|
        \usersPageCore ->
            let
                usersPage =
                    usersPageObserver
                        (Tuple.first usersPageCore)
                        page
            in
            [ Procedure.modify page <|
                \m -> { m | pageView = UsersPageView usersPageCore }
            , Procedure.jump usersPage <|
                \_ -> usersPageProcedures key page toast usersPage
            ]
    ]


usersPageObserver :
    ObserverId
    -> Observer m e Memory Event
    -> Observer m e UsersPageMemory UsersPageEvent
usersPageObserver oid =
    Observer.dig
        { get =
            \m1 ->
                case m1.pageView of
                    UsersPageView m2 ->
                        Just m2

                    _ ->
                        Nothing
        , set =
            \m2 m1 ->
                case m1.pageView of
                    UsersPageView _ ->
                        { m1
                            | pageView = UsersPageView m2
                        }

                    _ ->
                        m1
        , unwrap =
            \e1 ->
                case e1 of
                    UsersPageEvent e2 ->
                        Just e2

                    _ ->
                        Nothing
        , wrap = UsersPageEvent
        , id = oid
        }


usersPageProcedures :
    Key
    -> Observer m e Memory Event
    -> Observer m e Toast.Memory Toast.Event
    -> Observer m e UsersPageMemory UsersPageEvent
    -> Procedures m e
usersPageProcedures key page toast usersPage =
    [ Procedure.await usersPage <|
        \event _ ->
            case event of
                ClickShowEditAccountForm ->
                    [ Procedure.observe initEditAccountForm <|
                        \editAccountFormCore ->
                            let
                                editAccountForm =
                                    editAccountFormObserver
                                        (Tuple.first editAccountFormCore)
                                        usersPage
                            in
                            [ Procedure.modify usersPage <|
                                \m -> { m | editAccountForm = Just editAccountFormCore }
                            , Procedure.async <|
                                editAccountFormProcedures key page toast usersPage editAccountForm
                            , Procedure.jump usersPage <|
                                \_ -> usersPageProcedures key page toast usersPage
                            ]
                    ]

                _ ->
                    []
    ]


editAccountFormObserver :
    ObserverId
    -> Observer m e UsersPageMemory UsersPageEvent
    -> Observer m e EditAccountFormMemory EditAccountFormEvent
editAccountFormObserver oid =
    Observer.dig
        { get = .editAccountForm
        , set = \m2 m1 -> { m1 | editAccountForm = Just m2 }
        , unwrap =
            \e1 ->
                case e1 of
                    EditAccountFormEvent e2 ->
                        Just e2

                    _ ->
                        Nothing
        , wrap = EditAccountFormEvent
        , id = oid
        }


editAccountFormProcedures :
    Key
    -> Observer m e Memory Event
    -> Observer m e Toast.Memory Toast.Event
    -> Observer m e UsersPageMemory UsersPageEvent
    -> Observer m e EditAccountFormMemory EditAccountFormEvent
    -> Procedures m e
editAccountFormProcedures key page toast usersPage editAccountForm =
    let
        modifyForm f =
            Procedure.modify editAccountForm <|
                \m -> { m | form = f m.form }
    in
    [ Procedure.await editAccountForm <|
        \event memoryOnEvent ->
            case event of
                ChangeAccountId id ->
                    [ modifyForm <|
                        \m -> { m | id = id }
                    , Procedure.jump editAccountForm <|
                        \_ -> editAccountFormProcedures key page toast usersPage editAccountForm
                    ]

                ClickCancel ->
                    [ Procedure.modify usersPage <|
                        \m -> { m | editAccountForm = Nothing }
                    , Procedure.quit
                    ]

                ClickSubmitAccount ->
                    [ Procedure.modify editAccountForm <|
                        \m -> { m | isBusy = True }
                    , case EditAccount.fromForm memoryOnEvent.form of
                        Err _ ->
                            [ Procedure.modify editAccountForm <|
                                \m ->
                                    { m
                                        | isBusy = False
                                        , showError = True
                                    }
                            , Procedure.jump editAccountForm <|
                                \_ -> editAccountFormProcedures key page toast usersPage editAccountForm
                            ]
                            |> Procedure.batch
                        Ok editAccount ->
                            [ EditAccount.request editAccount ReceiveEditAccountResp editAccountForm
                                |> Procedure.mapCmd EditAccountCommand
                            , Procedure.await editAccountForm <|
                                \event2 _ ->
                                    case event2 of
                                        ReceiveEditAccountResp (Err err) ->
                                            [ Toast.pushHttpError err toast
                                                |> Procedure.mapCmd ToastCommand
                                            , Procedure.modify editAccountForm <|
                                                \m ->
                                                    { m | isBusy = False }
                                            , Procedure.jump editAccountForm <|
                                                \_ ->
                                                    editAccountFormProcedures key page toast usersPage editAccountForm
                                            ]

                                        ReceiveEditAccountResp (Ok resp) ->
                                            [ Procedure.modify page <|
                                                \m -> { m | session = resp.session }
                                            , Procedure.modify usersPage <|
                                                \m -> { m | editAccountForm = Nothing }
                                            , Procedure.quit
                                            ]

                                        _ ->
                                            []
                            ]
                            |> Procedure.batch
                    ]
                _ ->
                    []
    ]



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_users--" ++ name)



{-
-- Page.Users


type alias PageUsersMemory =
    { session : Session
    , users : List ( ObserverId, UserForm )
    }


initPageUsers : Session -> PageUsersMemory
initPageUsers session =
    { session = session
    , users = []
    }


type alias UserForm =
    { user : User
    , newUserName : String
    }


initUserForm : User -> UserForm
initUserForm user =
    { user = user
    , newUserName = ""
    }


type alias User =
    { id : String
    , name : String
    }


type PageUsersEvent
    = PageUsersReceiveInitialUsers (Result HttpError (List User))
    | PageUsersUserFormEvent PageUsersUserFormEvent


type PageUsersUserFormEvent
    = PageUsersChangeNewUserName String
    | PageUsersClickRegisterNewUser
    | PageUsersReceiveRegisterNewUserResp (Result HttpError User)
    | PageUsersClickRemoveUser
    | PageUsersReceiveRemoveUserResp (Result HttpError ())


unwrapPageUsersUserFormEvent : Wrapper event PageUsersEvent -> event -> Maybe PageUsersUserFormEvent
unwrapPageUsersUserFormEvent { unwrap } e =
    case unwrap e of
        Just (PageUsersUserFormEvent a) ->
            Just a

        _ ->
            Nothing


type PageUsersCmd
    = PageUsersPushRoute Route
    | PageUsersRequestInitialUsers
    | PageUsersRequestRegisterNewUser
    | PageUsersRequestRemoveUser
    | PageUsersDisplayLog String


pageUsersProcedures :
    (PageUsersCmd -> cmd)
    -> Wrapper event PageUsersEvent
    -> Observer memory PageUsersMemory
    -> List (Procedure_ cmd memory event)
pageUsersProcedures toCmd wrapper pageUsers =
    let
        users =
            pageUsers
                |> Procedure.dig
                    { get = .users
                    , set = \u memory -> { memory | users = u }
                    }
    in
    [ Procedure.push pageUsers <|
        \_ _ -> toCmd PageUsersRequestInitialUsers
    , Procedure.await pageUsers wrapper.unwrap <|
        \event _ ->
            case event of
                PageUsersReceiveInitialUsers (Err err) ->
                    [ handlePageUsersHttpError toCmd pageUsers RouteUsers err
                    , Procedure.quit
                    ]

                PageUsersReceiveInitialUsers (Ok [ u1, u2, u3, u4, u5 ]) ->
                    [ Procedure.prepend users
                        (initUserForm u3)
                        (\userForm ->
                            [ Procedure.async <|
                                pageUsersUserFormProcedures toCmd wrapper pageUsers userForm
                            ]
                        )
                    , Procedure.appendList users (List.map initUserForm [ u4, u5 ]) <|
                        List.map
                            (\userForm ->
                                Procedure.async <|
                                    pageUsersUserFormProcedures toCmd wrapper pageUsers userForm
                            )
                    , Procedure.prependList users (List.map initUserForm [ u1, u2 ]) <|
                        List.map
                            (\userForm ->
                                Procedure.async <|
                                    pageUsersUserFormProcedures toCmd wrapper pageUsers userForm
                            )
                    ]

                _ ->
                    []
    , Procedure.modify users <|
        List.map
            (\( oid, a ) ->
                if a.user.id == user2.id then
                    ( oid, { a | user = { id = a.user.id, name = newUser2.name } } )

                else
                    ( oid, a )
            )
    , putPageUsersLog toCmd pageUsers "Loaded users"
    ]


pageUsersUserFormProcedures :
    (PageUsersCmd -> cmd)
    -> Wrapper event PageUsersEvent
    -> Observer memory PageUsersMemory
    -> Observer memory UserForm
    -> List (Procedure_ cmd memory event)
pageUsersUserFormProcedures toCmd wrapper pageUsers userForm =
    let
        users =
            pageUsers
                |> Procedure.dig
                    { get = .users
                    , set = \u memory -> { memory | users = u }
                    }
    in
    [ Procedure.await userForm (unwrapPageUsersUserFormEvent wrapper) <|
        \event _ ->
            case event of
                PageUsersChangeNewUserName name ->
                    [ Procedure.modify userForm <|
                        \memory -> { memory | newUserName = name }
                    , Procedure.jump global <|
                        \_ ->
                            pageUsersUserFormProcedures toCmd wrapper pageUsers userForm
                    ]

                PageUsersClickRegisterNewUser ->
                    [ Procedure.push userForm <|
                        \_ _ -> toCmd PageUsersRequestRegisterNewUser
                    , Procedure.await userForm (unwrapPageUsersUserFormEvent wrapper) <|
                        \event2 _ ->
                            case event2 of
                                PageUsersReceiveRegisterNewUserResp (Err err) ->
                                    [ handlePageUsersHttpError toCmd pageUsers RouteUsers err
                                    , Procedure.jump global <|
                                        \_ ->
                                            pageUsersUserFormProcedures toCmd wrapper pageUsers userForm
                                    ]

                                PageUsersReceiveRegisterNewUserResp (Ok user) ->
                                    [ Procedure.modify userForm <|
                                        \memory ->
                                            { memory | newUserName = "" }
                                    , Procedure.async
                                        [ Procedure.insertAfter users userForm (initUserForm user) <|
                                            pageUsersUserFormProcedures toCmd wrapper pageUsers
                                        ]
                                    , Procedure.jump global <|
                                        \_ ->
                                            pageUsersUserFormProcedures toCmd wrapper pageUsers userForm
                                    ]

                                _ ->
                                    []
                    ]

                PageUsersClickRemoveUser ->
                    [ Procedure.push userForm <|
                        \_ _ -> toCmd PageUsersRequestRemoveUser
                    , Procedure.await userForm (unwrapPageUsersUserFormEvent wrapper) <|
                        \event2 _ ->
                            case event2 of
                                PageUsersReceiveRemoveUserResp (Err err) ->
                                    [ handlePageUsersHttpError toCmd pageUsers RouteUsers err
                                    , Procedure.jump global <|
                                        \_ ->
                                            pageUsersUserFormProcedures toCmd wrapper pageUsers userForm
                                    ]

                                PageUsersReceiveRemoveUserResp (Ok ()) ->
                                    [ Procedure.remove users userForm
                                    , Procedure.quit
                                    ]

                                _ ->
                                    []
                    ]

                _ ->
                    []
    ]


handlePageUsersHttpError : (PageUsersCmd -> cmd) -> Observer memory PageUsersMemory -> Route -> HttpError -> Procedure_ cmd memory event
handlePageUsersHttpError toCmd pageUsers route err =
    Procedure.batch <|
        case err of
            LoginRequired ->
                [ Procedure.push pageUsers <|
                    \_ _ ->
                        toCmd <|
                            PageUsersPushRoute <|
                                RouteLogin { back = route }
                ]

            SomeError str ->
                [ putPageUsersLog toCmd pageUsers str
                ]


putPageUsersLog : (PageUsersCmd -> cmd) -> Observer memory PageUsersMemory -> String -> Procedure_ cmd memory event
putPageUsersLog toCmd pageUsers str =
    Procedure.push pageUsers <|
        \_ _ -> toCmd <| PageUsersDisplayLog <| "[Page.Users] " ++ str







-- Test data


user1 : User
user1 =
    { id = "1"
    , name = "user 1"
    }


user2 : User
user2 =
    { id = "2"
    , name = "user 2"
    }


newUser2 : User
newUser2 =
    { id = "2"
    , name = "new user 2"
    }


user3 : User
user3 =
    { id = "3"
    , name = "user 3"
    }


user4 : User
user4 =
    { id = "4"
    , name = "user 4"
    }


user5 : User
user5 =
    { id = "5"
    , name = "user 5"
    }


user6 : User
user6 =
    { id = "6"
    , name = "user 6"
    }


user7 : User
user7 =
    { id = "7"
    , name = "user 7"
    }


-}
