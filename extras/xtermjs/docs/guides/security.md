---
title: Security
category: Guides
---

Giving security advice for xterm.js integration developers depends alot on the packages used to build and run the application, whether it is a standalone desktop application or accessible over the internet. We cannot cover the variety of these possibilities here and keep focused on terminal aspects in general in conjunction with browser / web technologies.

## Are terminals dangerous?

Terminals are the most fundamental concept for user interaction on all common operating systems. Of course terminals are not dangerous per se, but they offer great power to anyone who can access it. With great power comes great responsibility, sadly not all access attempts are driven by good intentions. If you have decided to integrate a terminal component into your application, you have to be aware of basic access rules and their security implications. Your application now inherits the danger coming from unintended terminal access with wrong privileges, thus it should strictly follow security policies provided by the system and not try to hide or circumvent them (e.g. deploying a system service with root / administrator privileges to drive terminals along with your application is a really bad idea). If in doubt consider applying further guards to the terminal connection like explicit ssh logins.

Basic rules:
- think twice if you really need a terminal component in your application as it raises the security requirements
- never circumvent security measures provided by the system
- check privileges of your application and whether they apply to the terminal
- go as restrictive as possible, let application users change privileges if needed

## Concerns arising from xterm.js' technology stack

xterm.js provides the terminal logic written in Typescript which compiles down to Javascript. The screen output relies on semantics provided by a modern HTML engine. Both facts introduce several security concerns.

### Javascript

The runtime logic of xterm.js is standalone, thus has no dependencies to other third-party Javascript packages. [One of the reasons](https://github.com/xtermjs/xterm.js/wiki/Contributing#third-party-dependencies) we do this is to avoid attacks through malicious third-party code, that might have been added to a package we cannot control. This very strict handling seems a bit paranoid, but keep in mind, that unintended access to a running terminal cannot only harm the hosting system, but all systems a terminal session might be connected to. For xterm.js addons this strict rule is not applied (consult the addon maintainer). We are also less strict about first party addons included in the core repository in this regard.

For your application, most likely relying on third-party packages, we can only encourage you to apply very high security standards when dealing with foreign code, double checking a package's trustworthiness or to take appropriate steps if in doubt.

### HTML engine

By using web technologies for the screen output xterm.js basically inherits all security concerns of web browsers with all known attack vectors. Bad enough? No, it gets worse - by placing a terminal into a HTML document it exposes direct shell access to any Javascript code on that page. Also keystrokes for terminal input have to go through Javascript, thus sensitive data like passwords can be read by any JS code. Users with high security requirements might stop right here and question a terminal solution based on web technologies. And yes, they have many good reasons to do so. Still we are going to try to give some advice to integration developers to mitigate most concerns.

**Excursion: Hints for users with high security requirements**
- **Is the xterm.js application (incl. server component) self-controlled or from a trustworthy, secured and stable source?**  
  If you can answer that question with yes it is probably safe to use the application as intended. Just keep in mind that even for self-deployed solutions the code already can be compromised, thus code audits might be needed to further backup the trust.
- **Does the xterm.js application load many third-party resources?**  
  This is quite a common deployment pattern in modern web development, probably most remote applications that offer terminal interaction via xterm.js are built that way. The problem for terminal I/O here comes from the question whether those third-party additions can be trusted. For high security demands this should always be seen as suspicious and not be used unless you have a security report stating otherwise. This also applies to CDNs, they have to be part of the trust chain.
- **Are there clear no-gos?**  
  Yes, probably more than "good conditions". Basically, if you cannot get satisfying answers from the two questions above, the application is more than suspicious and should not be used. Strong indicators, that an application should not be used, are:
    - There are resources with no valid transport encryption (no TLS or broken certificate).
    - The application updates content from third-party resources at runtime (e.g. embedded ads).
    - The application retrieves third-party content of unknown trust.
    - Insecure CDN resources.
    - The application has known security issues (e.g. CVE). Of course this depends on the quality of the issue, but as soon as XSS or Javascript in general is involved, avoid that application until it gets fixed.
    - The terminal data has to go through a server component of unkown trust. Never use those popular bash "one-liners" for sharing terminal content over an unknown server. If you really have to (e.g for some quick presentations), make sure to only give read access to non-sensitive data. If write access or sensitive data is involved, always use trustworthy services. Always try to avoid actions that might further escalate the risk (e.g. password inputs).


## Gain back users trust - harden your application

Lets look at the concerns more in detail and see if we can identify some basic rules to not put every system connected to xterm.js in danger on the first sight.

Note: The following was written with the most security challenging setup in mind - xterm.js embedded on some webpage accessible through the internet. More contained applications like container / cloud services or standalone desktop programs still share the basic concerns but are less likely affected by others.


### 1. Securing the HTML/JS content in general

We strongly advise you to read through the [OWASP Cheat Sheet Series](https://cheatsheetseries.owasp.org/), which contains an extensive list of known security issues and mitigations. The list gets updated regularily.

Furthermore we want to emphasize some basic rules, that should apply to any cleanly written web application. They definitely should not be taken lightly in conjunction with an xterm.js integration:
- **Always use secure transports for remote resources.**  
  Ensure this for all remote content, that gets embedded into the terminal component's page. There are no excuses, if you cannot afford the slightly higher setup / maintenance burden stop the development right there.
- **Avoid third-party resources.**  
  As written above, third-party content next to the terminal component introduces potential threats and raises the security audit needs for the users. Try to restrain from embedding a nifty JS driven counter or other content thats not really needed for the basic functionality.
- **No autoupdating foreign content. No ads.**  
  As simple as that - if something blinks and tries to get your attention it probably wants to get your credit card number. If your application has to rely on ad funding, strictly separate the terminal component from the more flashy parts of your application, never put it next to it. Again refer to the cheat series above to further mitigate possible CSRF/XSS vectors across several pages of your application. Note that you cannot expect from the user to decide whether you properly secured some random iframe content.
- **No dynamic Javascript code updates at runtime.**  
  Don't span your application's control flow over several JS resources that might get reloaded/updated during runtime. This again is hard to track for users and should not be used for any JS code that resides next to the terminal component. Instead try to prebundle the needed JS resources into a static asset, that can be inspected and evaluated during security auditions.


### 2. XSS and xterm.js in particular

**HTML/JS to terminal attack vector**  
One of the main threats for the terminal component is that any JS code within reach of the component can manipulate xterm.js and the terminal I/O. Due to this, it is important to properly secure the component's scripting context, which typically spans the embedding HTML page. Special care has to be taken to mitigate cross-side-scripting attacks (XSS):
- always use secure methods to update DOM content, never use easy-to-go methods that carry the risk of placing new JS code into the scripting context (e.g. never use `innerHTML`)
- don't update/inject JS code during runtime purposefully (no `eval`, no `fetch(SOME_JS)`)
- watch out for third-party resources pulling JS code into the scripting context, if you don't control that resource then don't use it (e.g. no foreign webfont loader)
- avoid anything that complicates the scripting context access or that might be subject to weak browser security settings like embedded iframes or non HTML/JS/CSS media resources
- harden the scripting context further by applying strict origin control rules (e.g. CORS)

For complex single page applications (SPA) these rules are not always maintainable without big sacrifices in functionality. In that case put the terminal component into its own scripting context (e.g. a subpage) with a smaller feature set to reduce the chance of security holes. Again refer to the cheat series above to further mitigate possible CSRF/XSS vectors across several pages of your application.

**terminal to HTML/JS attack vector**  
Embedding terminal I/O into a HTML document introduces another threat for web applications - anything coming from the terminal might contain malicious data. Thus it is important to treat terminal data always as untrusted when dealing with it on the DOM-level. xterm.js itself takes great care in its renderers to not let any terminal data cross the data-Javascript border. There are several API methods that may expose terminal data directly to embedders for further consumption:
- `Terminal.onTitleChange`
- linkifier interface
- any `Terminal.buffer` access
- custom parser hooks to digest and present terminal data

Always make sure to apply the data of those methods in a secure way (e.g. open links presented from terminal side in a separate context, no `innerHTML`).


### 3. websockets

If your application relies on websocket transport to drive the terminal I/O, further security measures are needed to not put the terminal connection at risk.

**Warning: Never use the demo application and the attach addon of the core repository directly to build your websocket solution!**

The demo application of the core repository is only meant for local development purposes of xterm.js itself and does not contain any security measures. Note that a websocket does not share typical security features applied by the browser engine to other resources like origin restrictions / CORS. Thus it is important to further secure the websocket yourself:
- use secure transport (wss)
- use additional protocols on top of websocket messages to provide appropriate authorization and authentication mechanisms
