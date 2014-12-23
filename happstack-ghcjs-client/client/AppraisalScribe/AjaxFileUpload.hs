{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
module AppraisalScribe.AjaxFileUpload (ajaxFileUploadInit, ajaxFileUploadJS, ajaxFileUploadTag) where

import AppraisalScribe.JMacro
import Lucid
import Data.Text as Text (Text, pack, unpack)

default (Text)

-- Mon Oct 29 13:03:51 PDT 2012
-- Possible new method
-- http://www.zurb.com/playground/ajax_upload

-- This module lets you do file uploads without reloading the entire
-- page.  Derived from:
-- http://viralpatel.net/blogs/ajax-style-file-uploading-using-hidden-iframe/
--
-- Forms force a page reload.  Most ajax requests side-step this, but
-- the only way to let the user upload a file is to actually use an
-- upload form.  The function ajaxFileUpload expects a form with an
-- associated div, but that form, instead of having a true submit
-- button, should create a button that calls ajaxFileUpload.  The user
-- selects files using the normal form, but the submit action is
-- redirected here.
--
-- ajaxFileUpload then inserts a new iframe as a sibling of the form,
-- and sets the target of the form to point to that iframe, which is
-- html magic to redirect where the response page gets sent.  I added
-- the namespace (ns) argument so that multiple instances of this
-- would work.
--
-- So this module has the raw html and javascript that are
-- interlocked, but too hard to model with my haskell libraries at
-- this point. The init function creates all the interlocked
-- components from universal types.

-- The server handling component will need to search the upload
-- request for the key 'fileTag'.

type URI = String

ajaxFileUploadTag :: String
ajaxFileUploadTag = "fileUpload[]"

ajaxFileUploadInit :: URI -> Bool -> Text -> Text -> Text -> Text -> JExpr -> Html ()
ajaxFileUploadInit url multiple fileTag divId namespace buttonText reloadParent =
  ajaxFileUploadHtml url multiple divId fileTag buttonText js
  where js = ajaxFileUploadC url divId namespace reloadParent

ajaxFileUploadHtml :: URI -> Bool -> Text -> Text -> Text -> JStat -> Html ()
ajaxFileUploadHtml _url multiple divId fileTag buttonText js =
  form_ $ do input_ ([type_ "file", name_ fileTag] ++ multi)
             input_ [type_ "button", value_ buttonText, onclick_ (Text.pack . show . renderJs $ js)]
             br_ []
             div_ [id_ divId] $ ""
  where multi = if multiple then [multiple_ ""] else []


instance ToJExpr Text where
  toJExpr = toJExpr . Text.unpack

-- Not the reference to this.form.  This must be used in a form.
ajaxFileUploadC :: URI -> Text -> Text -> JExpr -> JStat
ajaxFileUploadC url divId namespace reloadParent =
  [jmacro|ajaxFileUpload(this.form, `(url)`, `(divId)`, `(namespace)`, `(reloadParent)`); return false;|]

ajaxFileUploadJS :: JStat
ajaxFileUploadJS =
  [jmacro|
    function !ajaxFileUpload(form, action_url, div_id, ns, reloadParent) {
        // Create the iframe...
        var iframe = document.createElement("iframe");
        var iframeName = ns + "upload_iframe";

        iframe.setAttribute("id", iframeName);
        iframe.setAttribute("name", iframeName);
        iframe.setAttribute("width", "0");
        iframe.setAttribute("height", "0");
        iframe.setAttribute("border", "0");
        iframe.setAttribute("style", "width: 0; height: 0; border: none;");
        iframe.setAttribute("cookie", document.cookie);

        // Add to document...
        form.parentNode.appendChild(iframe);
        window.frames[iframeName].name = iframeName;

        iframeId = document.getElementById(iframeName);

        // Add event...
        var eventHandler = function () {

                if (iframeId.detachEvent) { iframeId.detachEvent("onload", eventHandler); }
                else { iframeId.removeEventListener("load", eventHandler, false); }

                // Message from server...
                if (iframeId.contentDocument) {
                    content = iframeId.contentDocument.body.innerHTML;
                } else if (iframeId.contentWindow) {
                    content = iframeId.contentWindow.document.body.innerHTML;
                } else if (iframeId.document) {
                    content = iframeId.document.body.innerHTML;
                }

                document.getElementById(div_id).innerHTML = content;

                // Del the iframe...
                // setTimeout('iframeId.parentNode.removeChild(iframeId)', 250);
                reloadParent();
            };

        if (iframeId.addEventListener) { iframeId.addEventListener("load", eventHandler, true); }
        if (iframeId.attachEvent) { iframeId.attachEvent("onload", eventHandler); }

        // Set properties of form...
        form.setAttribute("target", iframeName);
        form.setAttribute("action", action_url);
        form.setAttribute("method", "post");
        form.setAttribute("enctype", "multipart/form-data");
        form.setAttribute("encoding", "multipart/form-data");

        // Submit the form...
        form.submit();

        document.getElementById(div_id).innerHTML = "Uploading...";
    }
  |]
