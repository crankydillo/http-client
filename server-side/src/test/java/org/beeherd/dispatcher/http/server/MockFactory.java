/**
* Copyright 2010 Samuel Cox
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied. See the License for the
* specific language governing permissions and limitations
* under the License.
*/
package org.beeherd.dispatcher.http.server;

import java.util.*;

import javax.servlet.http.HttpServletRequest;

import static org.mockito.Mockito.*;

public class MockFactory {

  public static HttpServletRequest mockGetRequest() {
    HttpServletRequest req = mock(HttpServletRequest.class);

    when(req.getMethod()).thenReturn("GET");

    ArrayList names = new ArrayList();
    names.add("Content-Type");
    names.add("Accept");
    when(req.getHeaderNames()).thenReturn(Collections.enumeration(names));

    ArrayList contentTypeHeader = new ArrayList();
    contentTypeHeader.add("text/xml");
    when(req.getHeaders("Content-Type")).thenReturn(
            Collections.enumeration(contentTypeHeader));
    
    ArrayList acceptHeader = new ArrayList();
    acceptHeader.add("text/xml");
    acceptHeader.add("text/plain");
    when(req.getHeaders("Accept")).thenReturn(
            Collections.enumeration(acceptHeader));

    return req;
  }

  public static HttpServletRequest mockXmlPostRequest() {
    HttpServletRequest req = mock(HttpServletRequest.class);

    when(req.getMethod()).thenReturn("POST");

    ArrayList names = new ArrayList();
    names.add("Content-Type");
    when(req.getHeaderNames()).thenReturn(Collections.enumeration(names));

    ArrayList contentTypeHeader = new ArrayList();
    contentTypeHeader.add("text/xml");
    when(req.getHeaders("Content-Type")).thenReturn(
            Collections.enumeration(contentTypeHeader));
    
    return req;

  }

}
