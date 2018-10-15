{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.User
where

import Access
import Action
import Codec.Picture (DynamicImage)
import Frontend.Fragment.DelegationTab
import Frontend.Fragment.IdeaList
import Frontend.Fragment.Note
import Frontend.Prelude hiding ((</>), (<.>))
import Frontend.Validation
import Persistent.Api
    ( SetUserPass(SetUserPass)
    , SetUserDesc(SetUserDesc)
    )
import Persistent
    ( EQuery
    , DelegationListsMap(..)
    , DelegateeLists(..)
    , userDelegationListsMap
    , userDelegateListsMap
    , findUser
    , findIdeasByUserId
    , getIdeaStats
    , delegateInScope
    , delegationFull
    )

import qualified Data.Set as Set
import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF (Result(..))
import qualified Text.Digestive.Lucid.Html5 as DF


-- * misc

canDelegate :: CapCtx -> Bool
canDelegate ctx = CanDelegate `elem` capabilities ctx

profileContext :: (ActionPersist m, ActionUserHandler m, ActionLog m) => User -> m CapCtx
profileContext user = set capCtxUserProfile (Just user)
                    . set capCtxDelegateTo  (Just user)
                  <$> currentUserCapCtx


-- * page

-- | 9. User settings
data PageUserSettings = PageUserSettings User
  deriving (Eq, Show, Read)

instance Page PageUserSettings where
    -- The use settings page always goes to the profile of the current logged in user.
    -- However we do not want to rely on that so we check that the current user is the
    -- same as the edited user.
    isAuthorized = authNeedPage $ \cUser (PageUserSettings u) ->
        if cUser ^. _Id == u ^. _Id
            then accessGranted
            else accessDenied Nothing

-- | 8.1 User profile: Created ideas
data PageUserProfileCreatedIdeas =
        PageUserProfileCreatedIdeas CapCtx UserView ListItemIdeas [DelegationFull]
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas where
    isAuthorized = userPage -- profiles are public.  (see #695)

-- | 8.2 User profile: Votes from delegatees
data PageUserProfileUserAsDelegate =
        PageUserProfileUserAsDelegate CapCtx UserView DelegationListsMap [DelegationFull]
  deriving (Eq, Show, Read)

instance Page PageUserProfileUserAsDelegate where
    isAuthorized = userPage -- profiles are public.  (see #695)

-- | 8.X User profile: Votes to delegates
data PageUserProfileUserAsDelegatee =
        PageUserProfileUserAsDelegatee CapCtx UserView DelegationListsMap [DelegationFull]
  deriving (Eq, Show, Read)

instance Page PageUserProfileUserAsDelegatee where
    isAuthorized = userPage -- profiles are public.  (see #695)

-- | 8.X User profile: Editing the public profile
data EditUserProfile = EditUserProfile { _eupCapCtx :: CapCtx, _eupUser :: User }
  deriving (Eq, Show, Read)

makeLenses ''EditUserProfile

instance Page EditUserProfile where
    isAuthorized = authNeedCaps [CanEditUser] eupCapCtx

-- | 8.X Report user profile
data ReportUserProfile = ReportUserProfile User
  deriving (Eq, Show, Read)

instance Page ReportUserProfile where
    -- If you can view the profile of a user then you can report on it.
    -- Any user who is logged in can view the profile of any other user.
    isAuthorized = userPage


-- * templates

-- ** User Settings

data UserSettingData = UserSettingData
    { profileEmail    :: Maybe EmailAddress
    , profileOldPass  :: Maybe ST
    , profileNewPass1 :: Maybe ST
    , profileNewPass2 :: Maybe ST
    }
    deriving (Eq, Show)

-- This function checks that IF provided the password must be correct.
-- See checkPwdAllOrNothing which checks that the three passwords are present at once.
verifyUserPassIfExists :: ActionM m => Maybe ST -> m Bool
verifyUserPassIfExists Nothing    = pure True
verifyUserPassIfExists (Just pwd) = verifyUserPass pwd . view userPassword <$> currentUser

instance FormPage PageUserSettings where
    type FormPagePayload PageUserSettings = UserSettingData

    formAction _ = U.userSettings
    redirectOf (PageUserSettings u) _
        | has (Frontend.Prelude.userSettings . userSettingsPassword . _UserPassInitial) u = U.listSpaces
        | otherwise = U.userSettings

    makeForm (PageUserSettings user) =
          DF.check "Die neuen Passwörter passen nicht (Tippfehler?)" checkNewPassword
        . DF.check "Passwort-Felder sind nur teilweise ausgefüllt."  checkPwdAllOrNothing
        $ UserSettingData
            <$> ("email"         .:
                    optionalEmailField "Email" (user ^. userEmail))
            <*> ("old-password"  .:
                    -- while we need to check that the old password is the correct
                    -- one, we do not need to validate it against the rules for new passwords.
                    -- (the user could provide nothing, but 'checkPwdAllOrNothing' will catch that.)
                    DF.checkM "Das alte Passwort ist nicht korrekt" verifyUserPassIfExists
                    (DF.optionalText Nothing))
            <*> ("new-password1" .:
                    validateOptional "neues Passwort" passwordV (DF.optionalText Nothing))
            <*> ("new-password2" .:
                    validateOptional "neues Passwort (Wiederholung)" passwordV (DF.optionalText Nothing))
      where
        checkPwdAllOrNothing (UserSettingData _ Nothing  Nothing  Nothing)  = True
        checkPwdAllOrNothing (UserSettingData _ (Just _) (Just _) (Just _)) = True
        checkPwdAllOrNothing _                                              = False

        checkNewPassword u = profileNewPass1 u == profileNewPass2 u

    formPage v form p = do
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
            h1_ [class_ "main-heading", data_ "i18n" "user-settings"] "Einstellungen"
            form $ do
                label_ $ do
                    span_ [class_ "label-text", data_ "i18n" "user-email"] "E-mailadresse (optional)"
                    inputText_ [class_ "m-small"] -- FIXME should be inputEmail_
                        "email" v
                h2_ [class_ "label-header", data_ "i18n" "user-change-password"] "Passwort ändern"
                label_ $ do
                    span_ [class_ "label-text", data_ "i18n" "user-current-password"] "aktuelles Passwort"
                    inputPassword_ [class_ "m-small"]
                        "old-password" v
                label_ $ do
                    span_ [class_ "label-text", data_ "i18n" "user-new-password"] "neues Passwort"
                    inputPassword_ [class_ "m-small"]
                        "new-password1" v
                label_ $ do
                    span_ [class_ "label-text", data_ "i18n" "user-new-confirm"] "neues Passwort bestätigen"
                    inputPassword_ [class_ "m-small"]
                        "new-password2" v
                footer_ [class_ "form-footer"] $ do
                    input_ [type_ "submit", data_ "i18n" "[value]save-changes-button", value_ "Änderungen speichern"]



userSettings :: forall m . ActionM m => FormPageHandler m PageUserSettings
userSettings =
    formPageHandlerWithMsg
        (PageUserSettings <$> currentUser)
        changeUser
        "Die Änderungen wurden gespeichert."
  where
    changeUser :: UserSettingData -> m ()
    changeUser (UserSettingData memail _moldPass mnewPass1 mnewPass2) = do
        uid <- currentUserId
        updateUserEmail uid `mapM_` memail
        when (mnewPass1 /= mnewPass2) $ throwError500 "passwords do not match!"
        forM_ mnewPass1 $ encryptPassword >=> update . SetUserPass uid

userHeaderDiv :: (Monad m) => CapCtx -> Either User (User, [DelegationFull]) -> HtmlT m ()
userHeaderDiv _ (Left user) =
    div_ $ do
        h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
        p_ "Dieser Nutzer ist gelöscht"

userHeaderDiv ctx (Right (user, delegations)) =
    div_ $ do
        userHeaderDivCore user
        div_ [class_ "sub-header"] $ user ^. userDesc . html

        let btn lnk i18n = a_ [class_ "btn-cta heroic-cta m-large", href_ lnk, data_ "i18n" i18n]
            editProfileBtn = btn (U.editUserProfile user) "user-profile-edit" $ "+ Profil bearbeiten"

        div_ [class_ "heroic-btn-group"] $ do
            let caps = capabilities ctx
            when (any (`elem` caps) [CanDelegateInClass, CanDelegateInSchool]) $ do
                delegationButtons (ctx ^. capCtxUser) user delegations
            unless (isOwnProfile (ctx ^. capCtxUser) user) $
                btn (U.reportUser user) "user-profile-report" "melden"
            when (CanEditUser `elem` caps) $ do
                editProfileBtn

userHeaderDivCore :: User -> Monad m => HtmlT m ()
userHeaderDivCore user = do
    div_ [class_ "heroic-avatar"] $ userAvatarImg avatarDefaultSize user
    h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
    ul_ [class_ "role-badges"] $ do
        forM_ (user ^. userRoleSet . to Set.toList) $ \(r :: Role) ->
            li_ [class_ "badge"] $ r ^. uilabeled

commonIdeaSpaceDelegations :: User -> User -> EQuery [Delegation]
commonIdeaSpaceDelegations delegatee delegate = do
    let delegateeId = delegatee ^. _Id
    schoolDelegation <- delegateInScope delegateeId (DScopeIdeaSpace SchoolSpace)
    classDelegations <- forM (Set.toList $ commonSchoolClasses delegatee delegate)
        (delegateInScope delegateeId . DScopeIdeaSpace . ClassSpace)
    pure $ catMaybes (schoolDelegation:classDelegations)

-- | Delegation buttons works in a different way if the user opens
-- his/her own profile, or other users profile.  First argument
-- (`visiting`) is the (potential) delegatee, second argument (`visited`) the
-- (potential) delegate.
--
-- For the own profile clicking on the delegation button, the delegate selection
-- page is opened.
--
-- For other's profile, clicking on the delegation buttons mark the owner of
-- the profile as the delegate of the current user.
delegationButtons :: Monad m => User -> User -> [DelegationFull] -> HtmlT m ()
delegationButtons visiting visited delegations = do
    let ownProfile = isOwnProfile visiting visited
        isActiveDelegation = isJust . activeDelegation
        activeDelegation dscope = (`find` delegations)
            (\d -> d ^. delegationFullScope == dscope &&
                   d ^. delegationFullFrom . _Id == visiting ^. _Id &&
                   (ownProfile || d ^. delegationFullTo . _Id ==  visited ^. _Id))

        butGet path i18n = a_ [class_ "btn-cta heroic-cta m-large", href_ path, data_ "i18n" i18n]
        butPost = postButton_ [class_ "btn-cta heroic-cta m-large", jsReloadOnClick]
        ispaces = SchoolSpace : (ClassSpace <$> Set.toList (commonSchoolClasses visiting visited))

    forM_ ispaces $ \ispace -> do
        let dscope = DScopeIdeaSpace ispace
        div_ [class_ "heroic-cta-group"] $ do
            case (ownProfile, isActiveDelegation dscope) of
                (True, _) ->
                    butGet (U.createDelegation dscope) "" $ do
                        span_ [data_ "i18n" "user-delegates-for"] "Deine Beauftragung für "
                        span_ [] $ uilabel ispace
                (False, True) ->
                    butPost (U.withdrawDelegationOnIdeaSpace visited ispace)
                        ("Beauftragung für " <> uilabel ispace <> " entziehen")
                (False, False) ->
                    butPost (U.delegateVoteOnIdeaSpace visited ispace)
                        ("Für " <> uilabel ispace <> " beauftragen")

            -- display names of delegates (but only on own, not on delegate's profile)
            when (visiting ^. _Id == visited ^. _Id) $ do
                forM_ (activeDelegation dscope) $ \(DelegationFull _ _ delegate) -> do
                    p_ [class_ "sub-heading"] $ do
                        span_ [data_ "i18n" "user-delegates-idea"] $ do
                          "Derzeit stimmt für dich ab: "
                        a_ [href_ $ U.viewUserProfile delegate] $ do
                            delegate ^. userLogin . unUserLogin . html

-- | All 'DScopes' in which user watching the profile has delegated to the profile owner.
delegatedDScopes :: User -> DelegationListsMap -> [DScope]
delegatedDScopes delegatee (DelegationListsMap xs) = fullDScopeToDScope . fst <$> filter pr xs
  where
    pr :: (a, DelegateeLists) -> Bool
    pr (_, DelegateeLists ys) = delegatee `elem` (fst <$> ys)


-- ** User Profile: Created Ideas

data UserProfileTab
    = UserIdeasTab
    | UserDelegateesTab -- Delegatees give their vote to me, so i vote for them: "für wen stimme ich ab?" ("for whom do i vote?")
    | UserDelegatesTab  -- Delegates get their vote from me, so they vote for me: "wer stimmt für mich ab?" ("who votes for me?")
  deriving (Eq)

-- | FUTUREWORK: 'Frontend.Page.Topic.tabLink' shares some non-trivial code with this function that
-- could be factored out.
userProfileTab :: Monad m => CapCtx -> UserProfileTab -> User -> HtmlT m ()
userProfileTab ctx activeTab user = when (canCreateIdeas' || canDelegate') $ do
    div_ [class_ "heroic-tabs is-responsive"] $ allTabs False
    select_ [class_ "heroic-tabs-dropdown", onchange_ "window.location = this.value"] $ allTabs True
  where
    canCreateIdeas' = canCreateIdeas user
    canDelegate'    = canDelegate ctx

    allTabs dd = do
        when canCreateIdeas' $ tabLink dd UserIdeasTab      (U.viewUserProfile user) "user-created-ideas" "Erstellte Ideen"
        when canDelegate'    $ tabLink dd UserDelegateesTab (U.userDelegationsTo user) "user-who-delegations" "Für wen stimme ich ab?"
        when canDelegate'    $ tabLink dd UserDelegatesTab  (U.userDelegationsFrom user) "user-who-delegates" "Wer stimmt für mich ab?"

    tabLink True  = tabLinkDropdown
    tabLink False = tabLinkDiv

    tabLinkDropdown t path i18n
        = option_ $ [selected_ "true" | t == activeTab] <> [value_ . absoluteUriPath . U.relPath $ path] <> [data_ "i18n" i18n]

    tabLinkDiv t path i18n
        | t == activeTab = span_ [class_ "heroic-tab-item m-active", data_ "i18n" i18n]
        | otherwise      = a_    [class_ "heroic-tab-item", href_ path, data_ "i18n" i18n]

instance ToHtml PageUserProfileCreatedIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileCreatedIdeas ctx (DeletedUser user) _ideas _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileCreatedIdeas ctx (ActiveUser user) ideas delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            userProfileTab ctx UserIdeasTab user
        -- List of ideas
        when (canCreateIdeas user) . div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ toHtml ideas

-- | List all the created ideas for the given user.
createdIdeas :: (ActionPersist m, ActionUserHandler m, ActionLog m)
    => AUID User -> IdeasQuery -> m PageUserProfileCreatedIdeas
createdIdeas userId ideasQuery = do
    user <- mquery $ findUser userId
    ctx  <- profileContext user
    let visibleByCurrentUser idea =
            case idea ^. ideaLocation . ideaLocationSpace of
                SchoolSpace  -> True
                ClassSpace c ->
                    case ctx ^. capCtxUser . userRoleScope of
                        SchoolScope      -> True
                        ClassesScope cls -> c `Set.member` cls
    cUser <- currentUser
    equery (do
        ideas <- ListItemIdeas ctx (IdeaInUserProfile user) ideasQuery
              <$> (applyFilter ideasQuery <$>
                    (mapM getIdeaStats
                     =<< filter visibleByCurrentUser
                         <$> findIdeasByUserId userId))
        ds <- commonIdeaSpaceDelegations cUser user >>= mapM delegationFull
        pure $ PageUserProfileCreatedIdeas
            ctx
            (makeUserView user)
            ideas
            ds)


-- ** User Profile: User As Delegate

instance ToHtml PageUserProfileUserAsDelegate where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileUserAsDelegate ctx (DeletedUser user) _delegationListsMap _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileUserAsDelegate ctx (ActiveUser user) delegationListsMap delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            userProfileTab ctx UserDelegateesTab user
        when (canDelegate ctx) . div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    renderDelegations (UserDelegationPage ctx) delegationListsMap

userProfileUserDelegation
    :: (ActionPersist m, ActionUserHandler m, ActionLog m)
    => (CapCtx -> UserView -> DelegationListsMap -> [DelegationFull] -> page)
    -> (AUID User -> EQuery DelegationListsMap)
    -> AUID User -> m page
userProfileUserDelegation pageConstructor userDelegationsMap userId = do
    user <- mquery $ findUser userId
    ctx  <- profileContext user
    cUser <- currentUser
    equery $ do
        pageConstructor
            ctx
            (makeUserView user)
            <$> userDelegationsMap userId
            <*> (commonIdeaSpaceDelegations cUser user >>= mapM delegationFull)

userProfileUserAsDelegate :: (ActionPersist m, ActionUserHandler m, ActionLog m)
      => AUID User -> m PageUserProfileUserAsDelegate
userProfileUserAsDelegate =
    userProfileUserDelegation
        PageUserProfileUserAsDelegate
        userDelegationListsMap


-- ** User Profile: User as a delegatee

instance ToHtml PageUserProfileUserAsDelegatee where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileUserAsDelegatee ctx (DeletedUser user) _delegationListsMap _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileUserAsDelegatee ctx (ActiveUser user) delegationListsMap delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            userProfileTab ctx UserDelegatesTab user
        when (canDelegate ctx) . div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    renderDelegations (UserDelegationPage ctx) delegationListsMap

userProfileUserAsDelegatee :: (ActionPersist m, ActionUserHandler m, ActionLog m)
      => AUID User -> m PageUserProfileUserAsDelegatee
userProfileUserAsDelegatee =
    userProfileUserDelegation
        PageUserProfileUserAsDelegatee
        userDelegateListsMap


-- ** User Profile: Edit profile

data UserProfileUpdate = UserProfileUpdate
    { _profileAvatar :: Maybe DynamicImage
    , _profileDesc   :: Document
    }

makeLenses ''UserProfileUpdate

instance FormPage EditUserProfile where
    type FormPagePayload EditUserProfile = UserProfileUpdate

    formAction (EditUserProfile _ctx u) = U.editUserProfile u
    redirectOf (EditUserProfile _ctx u) _ = U.viewUserProfile u

    makeForm (EditUserProfile _ctx user) =
        UserProfileUpdate
        <$> ("avatar" .: DF.validateM validateImageFile DF.file)
        <*> ("desc"   .: validate "Beschreibung" markdownV (dftext . Just . unMarkdown $ user ^. userDesc))

    formPage v form p@(EditUserProfile ctx user) = do
        semanticDiv' [class_ "container-main container-narrow"] p $ do
            div_ [class_ "hero-unit"] $ do
                userHeaderDivCore user
                h2_ [class_ "sub-heading"] $ do
                    if isOwnProfile (ctx ^. capCtxUser) user
                        then span_ [data_ "i18n" "user-edit-own-profile"] "Eigenes Nutzerprofil bearbeiten"
                        else do
                          span_ [data_ "i18n" "user-edit-profile"] "Nutzerprofil von "
                          (user ^. userLogin . unUserLogin . html)
                          span_ [data_ "i18n" "edit"] " bearbeiten"

            form $ do
                div_ $ do
                    p_ [class_ "label-text"] "Einige wichtige Hinweise zum Hochladen von Bildern."
                    div_ [class_ "info-text"] $ do
                        p_ $ do
                            "Das alte Bild wird beim hochladen überschrieben.  Ziehe dir bitte "
                            "jetzt zuerst eine Sicherheitskopie, wenn du es später noch brauchst."
                        p_ $ do
                            "Nach dem hochladen wird das neue Bild in Kreisform geschnitten. "
                            "Es sollte also nicht zu lang oder hoch sein und nichts wichtiges "
                            "in den Ecken zeigen."
                        p_ $ do
                            let dim = fromString . show . maximum $ avatarDefaultSize : avatarExtraSizes
                            "Das neue Bild sollte für optimale Qualität mindestens "
                            dim >> "x" >> dim >> " "
                            "Pixel haben."
                        p_ $ do
                            "Es darf nicht größer sein als " >> avatarMaxByteSize ^. html >> "."
                    label_ $ do
                        span_ [class_ "label-text", data_ "i18n" "user-profile-picture"] "Profilbild"
                        DF.inputFile "avatar" v
                        br_ nil
                        br_ nil
                    label_ $ do
                        span_ [class_ "label-text", data_ "i18n" "user-description"] "Beschreibung"
                        inputTextArea_ [placeholder_ "..."] Nothing Nothing "desc" v
                    footer_ [class_ "form-footer"] $ do
                        input_ [type_ "submit", data_ "i18n" "[value]save-changes-button", value_ "Änderungen speichern"]
                        cancelButton p ()

validateImageFile :: (Monad n, ActionM m) => Maybe FilePath -> m (DF.Result (HtmlT n ()) (Maybe DynamicImage))
validateImageFile = \case
    Nothing   -> pure $ DF.Success Nothing
    Just file -> do
        img <- readImageFile (cs file)
        pure $ case img of
            Nothing          -> DF.Success Nothing  -- FIXME: get rid of this double-'Maybe'; see
                                                    -- documentation of 'readImageFile'
            Just (Right pic) -> DF.Success (Just pic)
            Just (Left _)    -> DF.Error "Die ausgewählte Datei ist kein Bild (jpg, png, gif, ...)"
                                -- (everything that juicy-pixles can read is allowed here)


editUserProfile :: ActionM m => AUID User -> FormPageHandler m EditUserProfile
editUserProfile uid = formPageHandlerWithMsg
    (do user <- mquery (findUser uid)
        ctx  <- profileContext user
        pure $ EditUserProfile ctx user
    )
    (\up -> do
        update . SetUserDesc uid $ up ^. profileDesc
        saveAvatar uid `mapM_` (up ^. profileAvatar)
    )
    "Die Änderungen wurden gespeichert."


-- ** User profile: Report user

reportUserNote :: Note User
reportUserNote = Note
    { noteHeaderText                = ("Report: " <>) . view (userLogin . unUserLogin)
    , noteExplanation               = Just "Hier kannst du ein Nutzerprofil wegen eines verletzenden oder anstößigen Inhalts beim Moderationsteam melden. Das Team erhält eine Benachrichtigung und wird die Idee schnellstmöglich überprüfen. Bitte gib unten einen Grund an, warum du den Inhalt für anstößig oder verletzend hältst."
    , noteLabelText                 = "Warum möchtest du das Nutzerprofil melden?"
    , noteFieldNameInValiationError = "Begründung"
    , noteI18n                      = "user-report"
    , noteHeaderTextLeft            = const "Report: "
    , noteHeaderTextRight           = view (userLogin . unUserLogin)
    , noteHeaderTextMiddle          = const ""
    }

instance FormPage ReportUserProfile where
    type FormPagePayload ReportUserProfile = Document

    formAction (ReportUserProfile user) = U.reportUser user
    redirectOf (ReportUserProfile user) _ = U.viewUserProfile user

    makeForm ReportUserProfile{} =
        noteFormInput reportUserNote Nothing

    formPage v form p@(ReportUserProfile user) =
        semanticDiv p $ do
            noteForm reportUserNote v form user

reportUser :: AUID User -> ActionM m => FormPageHandler m ReportUserProfile
reportUser userId = formPageHandlerWithMsg
    (ReportUserProfile <$> mquery (findUser userId))
    (Action.reportUser userId)
    "Das Nutzerprofil wurde der Moderation gemeldet."
