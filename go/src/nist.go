
package main

import (
 "encoding/json"
 "fmt"
 "io"
 "log"
 "net/http"
)

type NistExternal struct {
 SourceId string
 StatusCode int
 Value string
}

type NistListValue struct {
 Uri string
 Type string
 Value string
}

type NistPulse struct {
 Uri string
 Version string
 CipherSuite int
 Period int
 CertificateId string
 ChainIndex int
 PulseIndex int
 TimeStamp string
 LocalRandomValue string
 External NistExternal
 ListValues []NistListValue
 PrecommitValue string
 StatusCode int
 Signaturevalue string
 OutputValue string
}

type NistBaecon struct {
 Pulse NistPulse
}

func (nb *NistBaecon) Update() {
 url := "https://beacon.nist.gov/beacon/2.0/pulse/last";

 resp, err := http.Get(url)
 if err != nil {
 log.Fatalln(err)
 }
 defer resp.Body.Close()

 bodyBytes, _ := io.ReadAll(resp.Body)

 err = json.Unmarshal(bodyBytes, &nb)
 if err != nil {
 log.Fatalln("NistBaecon", err)
 }
}

func main() {
 var nb NistBaecon
 fmt.Println("Hello, World!")

 nb.Update()
 fmt.Println(nb.Pulse.OutputValue)
}
