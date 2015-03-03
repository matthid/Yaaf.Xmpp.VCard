// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
open System.IO
open NUnit.Framework
open FsUnit
open Swensen.Unquote
open Test.Yaaf.Xmpp.TestHelper
open Test.Yaaf.Xmpp
open Yaaf.Xmpp
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.Stream
open System.Threading.Tasks
open Yaaf.IO
open Yaaf.TestHelper
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.VCard


[<TestFixture>]
type ``Test-Yaaf-Xmpp-VCard-Parsing: Test that parsing works``() as this = 
    inherit XmlStanzaParsingTestClass()
    
    let discoTest stanzaString (info : IStanza) (elem : VCardAction) = 
        let newStanza = Parsing.createVCardElement info.Header.Id.Value info.Header.From info.Header.To elem
        this.GenericTest Parsing.vCardContentGenerator stanzaString newStanza
    
    [<Test>]
    member this.``Check that we can parse vcard element``() = 
        // NOTE: The order of the elements matter in this test, because the ordering is
        // irrelevant and we don't keep it.
        let stanza = "<iq id='v2' type='set'>
  <vCard xmlns='vcard-temp'>
    <FN>Peter Saint-Andre</FN>
    <N>
      <FAMILY>Saint-Andre</FAMILY>
      <GIVEN>Peter</GIVEN>
      <MIDDLE/>
    </N>
    <NICKNAME>stpeter</NICKNAME>
    <BDAY>1966-08-06</BDAY>
    <ADR>
      <WORK/>
      <EXTADD>Suite 600</EXTADD>
      <STREET>1899 Wynkoop Street</STREET>
      <LOCALITY>Denver</LOCALITY>
      <REGION>CO</REGION>
      <PCODE>80202</PCODE>
      <CTRY>USA</CTRY>
    </ADR>
    <ADR>
      <HOME/>
      <EXTADD/>
      <STREET/>
      <LOCALITY>Denver</LOCALITY>
      <REGION>CO</REGION>
      <PCODE>80209</PCODE>
      <CTRY>USA</CTRY>
    </ADR>
    <TEL><WORK/><VOICE/><NUMBER>303-308-3282</NUMBER></TEL>
    <TEL><WORK/><FAX/><NUMBER/></TEL>
    <TEL><WORK/><MSG/><NUMBER/></TEL>
    <TEL><HOME/><VOICE/><NUMBER>303-555-1212</NUMBER></TEL>
    <TEL><HOME/><FAX/><NUMBER/></TEL>
    <TEL><HOME/><MSG/><NUMBER/></TEL>
    <EMAIL><INTERNET/><PREF/><USERID>stpeter@jabber.org</USERID></EMAIL>
    <JABBERID>stpeter@jabber.org</JABBERID>
    <TITLE>Executive Director</TITLE>
    <ROLE>Patron Saint</ROLE>
    <ORG>
      <ORGNAME>XMPP Standards Foundation</ORGNAME>
      <ORGUNIT/>
    </ORG>
    <URL>http://www.xmpp.org/xsf/people/stpeter.shtml</URL>
    <DESC>Check out my blog at https://stpeter.im/</DESC>
  </vCard>
</iq>"
        let info = this.Test stanza
        Parsing.isContentVCard info |> should be True
        let elem = Parsing.parseContentVCard info
        let expected =
            VCardAction.Set(
              { VCardInfo.Create("Peter Saint-Andre", VCardName.Create()) with
                  Name =
                    { VCardName.Create() with
                        Family = Some "Saint-Andre"
                        Given = Some "Peter"
                        Middle = Some "" }
                  Nickname = [ "stpeter" ]
                  Url = [ "http://www.xmpp.org/xsf/people/stpeter.shtml" ]
                  Birthday = [ System.DateTime.Parse "1966-08-06" ]
                  Organization = [ { Name = "XMPP Standards Foundation"; FirstUnit = ""; OtherUnits = [] } ]
                  Title = [ "Executive Director" ]
                  Role = [ "Patron Saint" ]
                  Telephone =
                    [ { VCardTelephone.Create "303-308-3282" with IsVoice = true; IsWork = true }
                      { VCardTelephone.Create "" with IsWork = true; IsFax = true }
                      { VCardTelephone.Create "" with IsWork = true; IsMsg = true }
                      { VCardTelephone.Create "303-555-1212" with IsVoice = true; IsHome = true }
                      { VCardTelephone.Create "" with IsHome = true; IsFax = true }
                      { VCardTelephone.Create "" with IsHome = true; IsMsg = true }  ]
                  Address = 
                    [ { VCardAddress.Empty with 
                         IsWork = true; ExtAdd = Some "Suite 600"; Street = Some "1899 Wynkoop Street"
                         Locality = Some "Denver"; Region = Some "CO"; PCode = Some "80202"
                         Country = Some "USA" }
                      { VCardAddress.Empty with 
                         IsHome = true; ExtAdd = Some ""; Street = Some ""
                         Locality = Some "Denver"; Region = Some "CO"; PCode = Some "80209"
                         Country = Some "USA" } ]
                  EMail = [ { VCardEMail.Create "stpeter@jabber.org" with IsInternet = true; IsPref = true } ]
                  JabberId = [ JabberId.Parse "stpeter@jabber.org" ]
                  Desc = [ "Check out my blog at https://stpeter.im/" ]
                   })
        test <@ expected = elem @>
        
        discoTest stanza info elem
    
    [<Test>]
    member this.``Check that we can parse empty disco item element``() = 
        let stanza = "<iq from='stpeter@jabber.org/roundabout'
    id='v3'
    to='jer@jabber.org'
    type='get'>
  <vCard xmlns='vcard-temp'/>
</iq>"
        let info = this.Test stanza
        Parsing.isContentVCard info |> should be True
        let elem = Parsing.parseContentVCard info
        elem |> should be (equal <| VCardAction.Request)
        discoTest stanza info elem
        
        