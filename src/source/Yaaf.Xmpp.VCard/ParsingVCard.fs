// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// parsing logic for XEP-0054: vcard-temp (http://xmpp.org/extensions/xep-0054.html)
namespace Yaaf.Xmpp.VCard

open System
open Yaaf.Xmpp
open Yaaf.Xmpp.XmlStanzas

type VCardBinPhoto =
    {
        Type : string
        BinVal : string
    }

type VCardName =
    {
        Family : string option
        Given : string option
        Middle : string option
        Prefix : string option
        Suffix : string option
    } with
    static member Create () =
        {
            Family = None
            Given = None
            Middle = None
            Prefix = None
            Suffix = None
        }
    
type VCardOrganization =
    {
        Name : string
        FirstUnit : string
        OtherUnits : string list
    } with
    member x.AllUnits = x.FirstUnit :: x.OtherUnits

type VCardAddress =
    {
        IsHome : bool
        IsWork : bool
        IsPostal : bool
        IsParcel : bool
        /// true when DOM and false when INTL, none when none is given
        IsDomOrIntl : bool option
        IsPref : bool
        Pobox : string option
        ExtAdd : string option
        Street : string option
        Locality : string option
        Region : string option
        PCode : string option
        Country : string option
    }
    static member Empty =
      { IsHome = false
        IsWork = false
        IsPostal = false
        IsParcel = false
        /// true when DOM and false when INTL, none when none is given
        IsDomOrIntl = None
        IsPref = false
        Pobox = None
        ExtAdd = None
        Street = None
        Locality = None
        Region = None
        PCode = None
        Country = None }
        
    
type VCardPhoto =
    | Base64 of VCardBinPhoto
    | Url of string
    
type VCardLabel =
    {
        IsHome : bool
        IsWork : bool
        IsPostal : bool
        IsParcel : bool
        /// true when DOM and false when INTL, none when none is given
        IsDomOrIntl : bool option
        IsPref : bool
        FirstLine : string
        OtherLines : string list 
    } with
    member x.AllLines = x.FirstLine :: x.OtherLines

    
type VCardTelephone =
    {
        IsHome : bool
        IsWork : bool
        IsVoice : bool
        IsFax : bool
        IsPager : bool
        IsMsg : bool
        IsCell : bool
        IsVideo : bool
        IsBbs: bool
        IsModem: bool
        IsIsdn: bool
        IsPcs: bool
        IsPref : bool
        Number : string
    }
    static member Create number =
      { IsHome = false
        IsWork = false
        IsVoice = false
        IsFax = false
        IsPager = false
        IsMsg = false
        IsCell = false
        IsVideo = false
        IsBbs = false
        IsModem = false
        IsIsdn = false
        IsPcs = false
        IsPref = false
        Number = number }

    
type VCardEMail =
    {
        IsHome : bool
        IsWork : bool
        IsInternet : bool
        IsPref : bool
        IsX400 : bool
        UserId : string
    }
    static member Create address =
      { IsHome = false
        IsWork = false
        IsInternet = false
        IsPref = false
        IsX400 = false
        UserId = address }
    
type VCardGeo =
    {
        Lat : string
        Lon : string
    }
    
type VCardCategory =
    {
        FirstKeyword : string
        OtherKeywords : string list
    } with 
    member x.AllKeywords = x.FirstKeyword :: x.OtherKeywords
    
type VCardSound =
    | Phonetic of string
    | BinaryBase64 of string
    | Url of string

type VCardPrivacy =
    | Public 
    | Private
    | Confidential
    
type VCardKey =
    {
        Type : string option
        Credentials : string
    }

type VCardReference =
    | VCardReference of VCardInfo
    | Url of string
and VCardInfo =
    {
        Version : System.Version
        /// Formatted or display name property.
        FormattedName : string
        /// Structured name property. Name components with multiple values must be specified as a comma separated list of values.
        Name : VCardName
        /// Nickname property. Multiple nicknames must be specified as a comma separated list value.
        Nickname : string list
        /// Photograph property. Value is either a BASE64 encoded binary value or a URI to the external content.
        Photo : VCardPhoto list
        /// Birthday property. Value must be an ISO 8601 formatted date or date/time value.
        Birthday : DateTime list
        /// Structured address property. Address components with multiple values must be specified as a comma separated list of values.
        Address : VCardAddress list
        /// Address label property.
        Label : VCardLabel list
        /// Telephone number property.
        Telephone : VCardTelephone list
        /// Email address property. Default type is INTERNET
        EMail : VCardEMail list
        /// JabberId of the user
        JabberId : JabberId list
        /// Mailer (e.g., Mail User Agent Type) property.
        Mailer : string list
        /// Time zone's Standard Time UTC offset. Value must be an ISO 8601 formatted UTC offset.
        Tz : string list
        /// Geographical position. Values are the decimal degress of
        /// LATitude and LONgitude. The value should be specified to 
        /// six decimal places.
        Geo : VCardGeo list
        /// Title property.
        Title : string list
        /// Role property.
        Role : string list
        /// Organization logo property. 
        Logo : VCardPhoto list

        /// Administrative agent property.
        Agent : VCardReference list
        ///  Organizational name and units property.
        Organization : VCardOrganization list
        /// Application specific categories property.
        Categories : VCardCategory list
        /// Commentary note property.
        Note : string list
        /// Identifier of product that generated the vCard property.
        Prodid : string list
        /// Last revised property. The value must be an ISO 8601 formatted UTC date/time.
        Rev : DateTime list
        /// Sort string property.
        SortString : string list
        /// Formatted name pronunciation property. The value is 
        /// either a textual phonetic pronunciation, a BASE64 
        /// encoded binary digital audio pronunciation or a URI to
        /// an external binary digital audio pronunciation.
        Sound : VCardSound list
        /// Unique identifier property. 
        Uid : string list
        /// Directory URL property.
        Url : string list
        ///  Privacy classification property.
        Class : VCardPrivacy list
        /// Authentication credential or encryption key property.
        Key : VCardKey list
        /// NOTE: the following element was added by the Jabber
        /// project (now XMPP Standards Foundation) to
        /// handle free-form descriptive text.
        Desc : string list
    } with
    static member Create (fn, name) =
        {
            Version = new Version (3, 0)
            FormattedName = fn
            Name  = name
        
            Nickname = []
            Photo = []
            Birthday = []
            Address = []
            Label = []
            Telephone = []
            EMail = []
            JabberId = []
            Mailer = []
            Tz = []
            Geo = []
            Title = []
            Role = []
            Logo = []
        
            Agent = []
            Organization = []
            Categories = []
            Note  = []
            Prodid = []
            Rev = []
            SortString = []
            Sound = []
            Uid = []
            Url = []
            Class = []
            Key = []
            Desc = []
        }

type VCardAction =
    | Request
    | Set of VCardInfo
    | Return of VCardInfo option

type VCardStanza = Stanza<VCardAction>

module Parsing = 
    open System.Xml.Linq
    open Yaaf.Xml
    open Yaaf.Xmpp.XmlStanzas.Parsing
    
    let vCardNs = "vcard-temp"
    open Yaaf.Xmpp.XmlStanzas.StanzaParseException
    open Yaaf.Helper

    // Iq stanzas
    let isContentVCard (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind (fun e -> e.Name = getXName "vCard" vCardNs)
        queryItem.IsSome 

    let getElemValue (elem:XElement) = elem.Value

    let tryFindAndGetElemValue (elem:XElement) elemName =
        elem.Elements(getXName elemName vCardNs)
        |> Seq.exactlyOneOrNoneExn (lazy elemExn elem "expected one or no FAMILY element")
        |> Option.map getElemValue
    
    let hasElem elem elemName = (tryFindAndGetElemValue elem elemName).IsSome

    let parseVCardName (elem:XElement) =
        {
            Family = tryFindAndGetElemValue elem "FAMILY"
            Given = tryFindAndGetElemValue elem "GIVEN"
            Middle = tryFindAndGetElemValue elem "MIDDLE"
            Prefix = tryFindAndGetElemValue elem "PREFIX"
            Suffix = tryFindAndGetElemValue elem "SUFFIX"
        }

    let parseVCardPhoto (elem:XElement) = 
        match tryFindAndGetElemValue elem "EXTVAL" with
        | Some externValue -> VCardPhoto.Url externValue
        | None ->
            let picType =
                match tryFindAndGetElemValue elem "TYPE" with
                | Some t -> t
                | None -> elemFail elem "expected to find EXTVAL or (TYPE and BINVAL) elements"
            let picData =
                match tryFindAndGetElemValue elem "BINVAL" with
                | Some t -> t
                | None -> elemFail elem "expected to find EXTVAL or (TYPE and BINVAL) elements"
            VCardPhoto.Base64 { VCardBinPhoto.Type = picType; VCardBinPhoto.BinVal = picData }

    let parseIso8601 (elem:string) = 
        // TODO: ISO 8601
        System.DateTime.Parse(elem, System.Globalization.CultureInfo.InvariantCulture)
        
    let parseVCardBirthday (elem:XElement) = 
        parseIso8601(elem.Value)
    let isDomOrIntl elem =
        if hasElem elem "DOM" then
            if hasElem elem "INTL" 
            then elemFail elem "expected to find DOM or INTL elements"
            else Some true
        else
            if hasElem elem "INTL"
            then Some false
            else None
    let parseVCardAddress (elem:XElement) = 
        {
            IsHome = hasElem elem "HOME"
            IsWork = hasElem elem "WORK" 
            IsPostal  = hasElem elem "POSTAL"
            IsParcel = hasElem elem "PARCEL"
            IsDomOrIntl = isDomOrIntl elem
            IsPref  = hasElem elem "PREF"
            Pobox  = tryFindAndGetElemValue elem "POBOX"
            ExtAdd = tryFindAndGetElemValue elem "EXTADD"
            Street = tryFindAndGetElemValue elem "STREET"
            Locality = tryFindAndGetElemValue elem "LOCALITY"
            Region = tryFindAndGetElemValue elem "REGION"
            PCode  = tryFindAndGetElemValue elem "PCODE"
            Country = tryFindAndGetElemValue elem "CTRY"
        }

    let parseVCardLabel (elem:XElement) = 
        let lines =
            elem.Elements(getXName "LINE" vCardNs)
            |> Seq.map getElemValue
            |> Seq.toList
        match lines with
        | h :: t ->
            {
                IsHome = hasElem elem "HOME"
                IsWork = hasElem elem "WORK" 
                IsPostal  = hasElem elem "POSTAL"
                IsParcel = hasElem elem "PARCEL"
                IsDomOrIntl = isDomOrIntl elem
                IsPref  = hasElem elem "PREF"
                FirstLine = h
                OtherLines = t
            }
        | _ -> elemFail elem "expected at least one LINE element"

    let parseVCardTelephone (elem:XElement) = 
        {
            IsHome = hasElem elem "HOME"
            IsWork = hasElem elem "WORK" 
            IsVoice= hasElem elem "VOICE" 
            IsFax = hasElem elem "FAX" 
            IsPager = hasElem elem "PAGER" 
            IsMsg = hasElem elem "MSG" 
            IsCell = hasElem elem "CELL" 
            IsVideo = hasElem elem "VIDEO" 
            IsBbs = hasElem elem "BBS" 
            IsModem = hasElem elem "MODEM" 
            IsIsdn = hasElem elem "ISDN" 
            IsPcs = hasElem elem "PCS" 
            IsPref = hasElem elem "PREF" 
            Number = 
                match tryFindAndGetElemValue elem "NUMBER" with
                | Some t -> t
                | None -> elemFail elem "expected to find NUMBER element"
        }

    let parseVCardEMail (elem:XElement) = 
        {
            IsHome = hasElem elem "HOME"
            IsWork = hasElem elem "WORK" 
            IsInternet = hasElem elem "INTERNET" 
            IsPref = hasElem elem "PREF" 
            IsX400 = hasElem elem "X400" 
            UserId =
                match tryFindAndGetElemValue elem "USERID" with
                | Some t -> t
                | None -> elemFail elem "expected to find USERID element"
        }

    let parseVCardGeo (elem:XElement) = 
        {
            Lat =
                match tryFindAndGetElemValue elem "LAT" with
                | Some t -> t
                | None -> elemFail elem "expected to find LAT element"
            Lon = 
                match tryFindAndGetElemValue elem "LON" with
                | Some t -> t
                | None -> elemFail elem "expected to find LON element"
        }

    let parseVCardOrganization (elem:XElement) = 
        let units =
            elem.Elements(getXName "ORGUNIT" vCardNs)
            |> Seq.map getElemValue
            |> Seq.toList
        match units with
        | h :: t ->
            {
                Name = 
                    match tryFindAndGetElemValue elem "ORGNAME" with
                    | Some t -> t
                    | None -> elemFail elem "expected to find ORGNAME element"
                FirstUnit = h
                OtherUnits = t
            }
        | _ -> elemFail elem "expected at least one ORGUNIT element"

    let parseVCardCategories (elem:XElement) = 
        let keywords =
            elem.Elements(getXName "KEYWORD" vCardNs)
            |> Seq.map getElemValue
            |> Seq.toList
        match keywords with
        | h :: t ->
            {
                FirstKeyword = h
                OtherKeywords = t
            }
        | _ -> elemFail elem "expected at least one KEYWORD element"

    let parseVCardRev (elem:XElement) = 
        parseIso8601(elem.Value)

    let parseVCardSound (elem:XElement) = 
        match tryFindAndGetElemValue elem "PHONETIC" with
        | Some t -> VCardSound.Phonetic t
        | None -> 
            match tryFindAndGetElemValue elem "BINVAL" with
            | Some t -> VCardSound.BinaryBase64 t
            | None -> 
                match tryFindAndGetElemValue elem "EXTVAL" with
                | Some t -> VCardSound.Url t
                | None -> 
                    elemFail elem "expected to find PHONETIC or BINVAL or EXTVAL element"

    let parseVCardClass (elem:XElement) = 
        if hasElem elem "PUBLIC" then VCardPrivacy.Public
        elif hasElem elem "PRIVATE" then VCardPrivacy.Private
        elif hasElem elem "CONFIDENTIAL" then VCardPrivacy.Confidential
        else elemFail elem "expected to find PUBLIC or PRIVATE or CONFIDENTIAL element"

    let parseVCardKey (elem:XElement) = 
        {
            Type = tryFindAndGetElemValue elem "TYPE"
            Credentials =  
                match tryFindAndGetElemValue elem "CRED" with
                | Some t -> t
                | None -> 
                    elemFail elem "expected to find CRED element"
        }
        
    let rec parseVCardAgent (elem:XElement) = 
        match tryFindAndGetElemValue elem "EXTVAL" with
        | Some extVal -> VCardReference.Url extVal
        | None ->
            let inner = parseContentVCardElem (elem.Element(getXName "vCard" vCardNs))
            VCardReference.VCardReference inner
    and parseContentVCardElem (elem:XElement) = 
        if elem.Name.NamespaceName <> vCardNs then elemFail elem "invalid vcard element!"
        if elem.Name.LocalName <> "vCard" then elemFail elem "invalid vcard element!"
        {
            Version = new Version (3, 0)
            FormattedName = 
                elem.Elements(getXName "FN" vCardNs)
                |> Seq.exactlyOneExn (lazy elemExn elem "FN must be available")
                |> getElemValue
            Name = 
                elem.Elements(getXName "N" vCardNs)
                |> Seq.exactlyOneExn (lazy elemExn elem "N must be available")
                |> parseVCardName
            Nickname = 
                elem.Elements(getXName "NICKNAME" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Photo = 
                elem.Elements(getXName "PHOTO" vCardNs)
                |> Seq.map parseVCardPhoto
                |> Seq.toList
            Birthday = 
                elem.Elements(getXName "BDAY" vCardNs)
                |> Seq.map parseVCardBirthday
                |> Seq.toList
            Address =
                elem.Elements(getXName "ADR" vCardNs)
                |> Seq.map parseVCardAddress
                |> Seq.toList
            Label = 
                elem.Elements(getXName "LABEL" vCardNs)
                |> Seq.map parseVCardLabel
                |> Seq.toList
            Telephone = 
                elem.Elements(getXName "TEL" vCardNs)
                |> Seq.map parseVCardTelephone
                |> Seq.toList
            EMail = 
                elem.Elements(getXName "EMAIL" vCardNs)
                |> Seq.map parseVCardEMail
                |> Seq.toList
            JabberId = 
                elem.Elements(getXName "JABBERID" vCardNs)
                |> Seq.map getElemValue
                |> Seq.map JabberId.Parse
                |> Seq.toList
            Mailer = 
                elem.Elements(getXName "MAILER" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Tz = 
                elem.Elements(getXName "TZ" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Geo = 
                elem.Elements(getXName "GEO" vCardNs)
                |> Seq.map parseVCardGeo
                |> Seq.toList
            Title = 
                elem.Elements(getXName "TITLE" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Role = 
                elem.Elements(getXName "ROLE" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Logo = 
                elem.Elements(getXName "LOGO" vCardNs)
                |> Seq.map parseVCardPhoto
                |> Seq.toList
        
            Agent = 
                elem.Elements(getXName "AGENT" vCardNs)
                |> Seq.map parseVCardAgent
                |> Seq.toList
            Organization = 
                elem.Elements(getXName "ORG" vCardNs)
                |> Seq.map parseVCardOrganization
                |> Seq.toList
            Categories = 
                elem.Elements(getXName "CATEGORIES" vCardNs)
                |> Seq.map parseVCardCategories
                |> Seq.toList
            Note  = 
                elem.Elements(getXName "NOTE" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Prodid = 
                elem.Elements(getXName "PRODID" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Rev = 
                elem.Elements(getXName "REV" vCardNs)
                |> Seq.map parseVCardRev
                |> Seq.toList
            SortString =
                elem.Elements(getXName "SORT-STRING" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Sound =
                elem.Elements(getXName "SOUND" vCardNs)
                |> Seq.map parseVCardSound
                |> Seq.toList
            Uid =
                elem.Elements(getXName "UID" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Url =
                elem.Elements(getXName "URL" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
            Class =
                elem.Elements(getXName "CLASS" vCardNs)
                |> Seq.map parseVCardClass
                |> Seq.toList
            Key =
                elem.Elements(getXName "KEY" vCardNs)
                |> Seq.map parseVCardKey
                |> Seq.toList
            Desc =
                elem.Elements(getXName "DESC" vCardNs)
                |> Seq.map getElemValue
                |> Seq.toList
        }

    let parseContentVCard (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then parseFail stanza "expected iq stanza"
        else
            stanza.Contents.Children 
            |> Seq.filter (fun e -> e.Name = getXName "vCard" vCardNs)
            |> Seq.map (fun elem ->
                if not elem.HasElements then
                    // error, no result or request
                    assert (stanza.Header.Type = Some "get" || stanza.Header.Type = Some "result" || stanza.Header.Type = Some "error" )
                    match stanza.Header.Type with
                    | Some "get" -> VCardAction.Request
                    | Some "result" -> VCardAction.Return None
                    | _-> failwith "not expected type"
                else
                    let contents = parseContentVCardElem elem
                    match stanza.Header.Type with
                    | Some "get" -> failwith "contents not expected when requesting vcard"
                    | Some "result" -> VCardAction.Return (Some contents)
                    | Some "set" -> VCardAction.Set contents
                    | _-> failwith "not expected stanza type")
            |> Seq.exactlyOneExn (lazy parseExn stanza "")
    
    let parseVCardStanza ns (elem:XElement) = 
        parseGenericStanza ns parseContentVCard elem

    let createElemWithName (name:string) (v:string) = 
        [ v :> obj ] |> getXElemWithChilds (getXName name vCardNs)
    let createOptionalElemWithName (name:string) (v:string option) =
        match v with
        | Some v -> [ createElemWithName name v ]
        | None -> []
    let createOptionalEmptyElemWithName (name:string) (create:bool) =
        if create then
            [ getXElemWithChilds (getXName name vCardNs) [] ]
        else []
    let createVCardNameElem (name: VCardName) =
        [
            yield! createOptionalElemWithName "FAMILY" name.Family
            yield! createOptionalElemWithName "GIVEN" name.Given
            yield! createOptionalElemWithName "MIDDLE" name.Middle
            yield! createOptionalElemWithName "PREFIX" name.Prefix
            yield! createOptionalElemWithName "SUFFIX" name.Suffix
        ] |> getXElemWithChilds (getXName "N" vCardNs)

    let createVCardPhotoElem (elemName:string) (photo: VCardPhoto) =
        [
            match photo with
            | VCardPhoto.Base64 enc ->
                yield createElemWithName "TYPE" enc.Type
                yield createElemWithName "BINVAL" enc.BinVal
            | VCardPhoto.Url url ->
                yield createElemWithName "EXTVAL" url
        ] |> getXElemWithChilds (getXName elemName vCardNs)
    let createIso8601 (date:DateTime) =
        date.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture)
    let createVCardBirthdayElem (date: DateTime) =
        [
            yield createIso8601 date :> obj
        ] |> getXElemWithChilds (getXName "BDAY" vCardNs)
    let createDomOrIntlElement isDomOrIntl =
        match isDomOrIntl with
        | None -> []
        | Some true ->
            createOptionalEmptyElemWithName "DOM" true
        | Some false ->
            createOptionalEmptyElemWithName "INTL" true
    let createVCardAddressElem (address: VCardAddress) =
        [
            yield! createOptionalEmptyElemWithName "HOME" address.IsHome
            yield! createOptionalEmptyElemWithName "WORK" address.IsWork
            yield! createOptionalEmptyElemWithName "POSTAL" address.IsPostal
            yield! createOptionalEmptyElemWithName "PARCEL" address.IsParcel
            yield! createDomOrIntlElement address.IsDomOrIntl
            yield! createOptionalEmptyElemWithName "PREF" address.IsPref
            yield! createOptionalElemWithName "POBOX" address.Pobox
            yield! createOptionalElemWithName "EXTADD" address.ExtAdd
            yield! createOptionalElemWithName "STREET" address.Street
            yield! createOptionalElemWithName "LOCALITY" address.Locality
            yield! createOptionalElemWithName "REGION" address.Region
            yield! createOptionalElemWithName "PCODE" address.PCode
            yield! createOptionalElemWithName "CTRY" address.Country
        ] |> getXElemWithChilds (getXName "ADR" vCardNs)

    let createVCardLabelElem (label: VCardLabel) =
        [
            yield! createOptionalEmptyElemWithName "HOME" label.IsHome
            yield! createOptionalEmptyElemWithName "WORK" label.IsWork
            yield! createOptionalEmptyElemWithName "POSTAL" label.IsPostal
            yield! createOptionalEmptyElemWithName "PARCEL" label.IsParcel
            yield! createDomOrIntlElement label.IsDomOrIntl
            yield! createOptionalEmptyElemWithName "PREF" label.IsPref
            yield! label.AllLines |> Seq.map (createElemWithName "LINE")
        ] |> getXElemWithChilds (getXName "LABEL" vCardNs)

    let createVCardTelephoneElem (tel: VCardTelephone) =
        [
            yield! createOptionalEmptyElemWithName "HOME" tel.IsHome
            yield! createOptionalEmptyElemWithName "WORK" tel.IsWork
            yield! createOptionalEmptyElemWithName "VOICE" tel.IsVoice
            yield! createOptionalEmptyElemWithName "FAX" tel.IsFax
            yield! createOptionalEmptyElemWithName "PAGER" tel.IsPager
            yield! createOptionalEmptyElemWithName "MSG" tel.IsMsg
            yield! createOptionalEmptyElemWithName "CELL" tel.IsCell
            yield! createOptionalEmptyElemWithName "VIDEO" tel.IsVideo
            yield! createOptionalEmptyElemWithName "BBS" tel.IsBbs
            yield! createOptionalEmptyElemWithName "MODEM" tel.IsModem
            yield! createOptionalEmptyElemWithName "ISDN" tel.IsIsdn
            yield! createOptionalEmptyElemWithName "PCS" tel.IsPcs
            yield! createOptionalEmptyElemWithName "PREF" tel.IsPref
            yield createElemWithName "NUMBER" tel.Number
        ] |> getXElemWithChilds (getXName "TEL" vCardNs)

    let createVCardEMailElem (email: VCardEMail) =
        [
            yield! createOptionalEmptyElemWithName "HOME" email.IsHome
            yield! createOptionalEmptyElemWithName "WORK" email.IsWork
            yield! createOptionalEmptyElemWithName "INTERNET" email.IsInternet
            yield! createOptionalEmptyElemWithName "PREF" email.IsPref
            yield! createOptionalEmptyElemWithName "X400" email.IsX400
            yield createElemWithName "USERID" email.UserId
        ] |> getXElemWithChilds (getXName "EMAIL" vCardNs)

    let createVCardGeoElem (geo: VCardGeo) =
        [
            yield createElemWithName "LAT" geo.Lat
            yield createElemWithName "LON" geo.Lon
        ] |> getXElemWithChilds (getXName "GEO" vCardNs)

    let createVCardOrganizationElem (org: VCardOrganization) =
        [
            yield createElemWithName "ORGNAME" org.Name
            yield! org.AllUnits |> Seq.map (createElemWithName "ORGUNIT")
        ] |> getXElemWithChilds (getXName "ORG" vCardNs)
        
    let createVCardCategoriesElem (cat: VCardCategory) =
        [
            yield! cat.AllKeywords |> Seq.map (createElemWithName "KEYWORD")
        ] |> getXElemWithChilds (getXName "CATEGORIES" vCardNs)
        
    let createVCardRevElem (rev: DateTime) =
        [
            yield createIso8601 rev :> obj
        ] |> getXElemWithChilds (getXName "REV" vCardNs)
        
    let createVCardSoundElem (sound: VCardSound) =
        let elemName, value =
            match sound with
            | VCardSound.BinaryBase64 enc ->
                "BINVAL", enc
            | VCardSound.Phonetic enc ->
                "PHONETIC", enc 
            | VCardSound.Url enc ->
                "EXTVAL", enc 
        [
            yield createElemWithName elemName value
        ] |> getXElemWithChilds (getXName "SOUND" vCardNs)

    let createVCardClassElem (privacy: VCardPrivacy) =
        let elemName =
            match privacy with
            | VCardPrivacy.Private ->
                "PUBLIC"
            | VCardPrivacy.Public ->
                "PRIVATE" 
            | VCardPrivacy.Confidential ->
                "CONFIDENTIAL"
        [
            yield! createOptionalEmptyElemWithName elemName true
        ] |> getXElemWithChilds (getXName "CLASS" vCardNs)

    let createVCardKeyElem (key: VCardKey) =
        [
            yield! createOptionalElemWithName "TYPE" key.Type
            yield createElemWithName "CRED" key.Credentials
        ] |> getXElemWithChilds (getXName "CLASS" vCardNs)

    let rec createVCardAgentElem (ref:VCardReference) = 
        let childs =
            match ref with
            | VCardReference.Url ext ->
                [
                    yield createElemWithName "EXTVAL" ext
                ]
            | VCardReference.VCardReference other ->
                [
                    yield createVCardInfoElement other
                ]
        childs |> getXElemWithChilds (getXName "AGENT" vCardNs)
    and createVCardInfoElement (vcardInfo:VCardInfo) =
        [
            yield createElemWithName  "FN" vcardInfo.FormattedName :> obj
            yield createVCardNameElem vcardInfo.Name :> obj
            yield! vcardInfo.Nickname |> Seq.map (createElemWithName "NICKNAME") |> Seq.cast<obj>
            yield! vcardInfo.Photo |> Seq.map (createVCardPhotoElem "PHOTO") |> Seq.cast<obj>
            yield! vcardInfo.Birthday |> Seq.map (createVCardBirthdayElem) |> Seq.cast<obj>
            yield! vcardInfo.Address |> Seq.map (createVCardAddressElem) |> Seq.cast<obj>
            yield! vcardInfo.Label |> Seq.map (createVCardLabelElem) |> Seq.cast<obj>
            yield! vcardInfo.Telephone |> Seq.map (createVCardTelephoneElem) |> Seq.cast<obj>
            yield! vcardInfo.EMail |> Seq.map (createVCardEMailElem) |> Seq.cast<obj>
            yield! vcardInfo.JabberId |> Seq.map (fun jid -> jid.BareId) |> Seq.map (createElemWithName "JABBERID") |> Seq.cast<obj>
            yield! vcardInfo.Mailer |> Seq.map (createElemWithName "MAILER") |> Seq.cast<obj>
            yield! vcardInfo.Tz |> Seq.map (createElemWithName "TZ") |> Seq.cast<obj>
            yield! vcardInfo.Geo |> Seq.map (createVCardGeoElem) |> Seq.cast<obj>
            yield! vcardInfo.Title |> Seq.map (createElemWithName "TITLE") |> Seq.cast<obj>
            yield! vcardInfo.Role |> Seq.map (createElemWithName "ROLE") |> Seq.cast<obj>
            yield! vcardInfo.Logo |> Seq.map (createVCardPhotoElem "LOGO") |> Seq.cast<obj>
            yield! vcardInfo.Agent |> Seq.map (createVCardAgentElem) |> Seq.cast<obj>
            yield! vcardInfo.Organization |> Seq.map (createVCardOrganizationElem) |> Seq.cast<obj>
            yield! vcardInfo.Categories |> Seq.map (createVCardCategoriesElem) |> Seq.cast<obj>
            yield! vcardInfo.Note |> Seq.map (createElemWithName "NOTE") |> Seq.cast<obj>
            yield! vcardInfo.Prodid |> Seq.map (createElemWithName "PRODID") |> Seq.cast<obj>
            yield! vcardInfo.Rev |> Seq.map (createVCardRevElem) |> Seq.cast<obj>
            yield! vcardInfo.SortString |> Seq.map (createElemWithName "SORT-STRING") |> Seq.cast<obj>
            yield! vcardInfo.Sound |> Seq.map (createVCardSoundElem) |> Seq.cast<obj>
            yield! vcardInfo.Uid |> Seq.map (createElemWithName "UID") |> Seq.cast<obj>
            yield! vcardInfo.Url |> Seq.map (createElemWithName "URL") |> Seq.cast<obj>
            yield! vcardInfo.Class |> Seq.map (createVCardClassElem) |> Seq.cast<obj>
            yield! vcardInfo.Key |> Seq.map (createVCardKeyElem) |> Seq.cast<obj>
            yield! vcardInfo.Desc |> Seq.map (createElemWithName "DESC") |> Seq.cast<obj>
        ] |> getXElemWithChilds (getXName "vCard" vCardNs)
    
    let createVCardStanzaElement (action: VCardAction) =
        match action with
        | VCardAction.Set vcardInfo | VCardAction.Return (Some vcardInfo) ->
            createVCardInfoElement vcardInfo
        | _ -> [] |> getXElemWithChilds (getXName "vCard" vCardNs)
        

    let vCardContentGenerator = ContentGenerator.SimpleGenerator createVCardStanzaElement
    let createVCardElement (id:string) (fromJid:JabberId option) (toJid:JabberId option) (data:VCardAction) = 
        let typeString = 
            match data with
            | Request -> "get"
            | Set _ -> "set"
            | VCardAction.Return _ -> "result"
        Stanza<_>.CreateGen vCardContentGenerator
          { To = toJid
            From = fromJid
            Id = Some id
            Type = Some typeString
            StanzaType = XmlStanzaType.Iq }
          data