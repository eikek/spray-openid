/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.eknet.spray.openid.model

import org.scalatest.WordSpec
import org.eknet.spray.openid.model

class RealmVerifyTest extends WordSpec {
  import spray.http.Uri

  implicit class UriAdds(uri: Uri) {
    def matchesRealm(realm: String) = uriMatchesRealm(uri, Uri(realm))
  }

  "A realm verifiyer" must {

    "properly find mismatches" in {
      assert(Uri("http://example.com/path").matchesRealm("http://example.com/path#frag") === false)
      assert(Uri("https://example.com/").matchesRealm("http://example.com/") === false)
      assert(Uri("http://example.com/").matchesRealm("https://example.com/") === false)
      assert(Uri("http://example.com:82").matchesRealm("https://example.com/") === false)
      assert(Uri("http://example.com").matchesRealm("https://example.com:88") === false)
      assert(Uri("https://example.com/path/other").matchesRealm("https://example.com/Path") === false)
    }

    "match correct urls" in {
      assert(Uri("http://example.com/path").matchesRealm("http://example.com/path") === true)
      assert(Uri("http://example.com:122/path").matchesRealm("http://example.com:122/path") === true)
      assert(Uri("https://example.com:9443/path/other").matchesRealm("https://example.com:9443/path") === true)
    }
  }
}
