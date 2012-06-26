(* ::Package:: *)

(* ::Title:: *)
(*Definitions*)


(* ::Subsection::Closed:: *)
(*Notice*)


(* ::Text:: *)
(*Mathematica cooperative energy detection and fading channel function definitions.*)
(*Copyright (C) 2012 Donagh Horgan.*)
(*Email: donaghh@rennes.ucc.ie.*)
(**)
(*This program is free software : you can redistribute it and/or modify*)
(*it under the terms of the GNU General Public License as published by*)
(*the Free Software Foundation, either version 3 of the License, or*)
(*(at your option) any later version.*)
(**)
(*This program is distributed in the hope that it will be useful,*)
(*but WITHOUT ANY WARRANTY; without even the implied warranty of*)
(*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the*)
(*GNU General Public License for more details.*)
(**)
(*You should have received a copy of the GNU General Public License*)
(*along with this program. If not, see < http : // www.gnu.org/licenses/ > .*)


(* ::Subsection::Closed:: *)
(*GNU General Public License (GPL)*)


(* ::Text:: *)
(*GNU GENERAL PUBLIC LICENSE*)
(*Version 3, 29 June 2007*)
(**)
(* Copyright (C) 2007 Free Software Foundation, Inc. < http : // fsf.org/ >*)
(* Everyone is permitted to copy and distribute verbatim copies*)
(* of this license document, but changing it is not allowed.*)
(**)
(*Preamble*)
(**)
(*The GNU General Public License is a free, copyleft license for*)
(*software and other kinds of works.*)
(**)
(*The licenses for most software and other practical works are designed*)
(*to take away your freedom to share and change the works.By contrast,*)
(*the GNU General Public License is intended to guarantee your freedom to*)
(*share and change all versions of a program-- to make sure it remains free*)
(*software for all its users.We, the Free Software Foundation, use the*)
(*GNU General Public License for most of our software; it applies also to*)
(*any other work released this way by its authors.You can apply it to*)
(*your programs, too.*)
(**)
(*When we speak of free software, we are referring to freedom, not*)
(*price.Our General Public Licenses are designed to make sure that you*)
(*have the freedom to distribute copies of free software (and charge for*)
(*them if you wish), that you receive source code or can get it if you*)
(*want it, that you can change the software or use pieces of it in new*)
(*free programs, and that you know you can do these things.*)
(**)
(*To protect your rights, we need to prevent others from denying you*)
(*these rights or asking you to surrender the rights.Therefore, you have*)
(*certain responsibilities if you distribute copies of the software, or if*)
(*you modify it : responsibilities to respect the freedom of others.*)
(**)
(*For example, if you distribute copies of such a program, whether*)
(*gratis or for a fee, you must pass on to the recipients the same*)
(*freedoms that you received.You must make sure that they, too, receive*)
(*or can get the source code.And you must show them these terms so they*)
(*know their rights.*)
(**)
(*Developers that use the GNU GPL protect your rights with two steps :*)
(*(1) assert copyright on the software, and (2) offer you this License*)
(*giving you legal permission to copy, distribute and/or modify it.*)
(**)
(*For the developers' and authors' protection, the GPL clearly explains*)
(*that there is no warranty for this free software.For both users' and*)
(*authors' sake, the GPL requires that modified versions be marked as*)
(*changed, so that their problems will not be attributed erroneously to*)
(*authors of previous versions.*)
(**)
(*Some devices are designed to deny users access to install or run*)
(*modified versions of the software inside them, although the manufacturer*)
(*can do so.This is fundamentally incompatible with the aim of*)
(*protecting users' freedom to change the software.The systematic*)
(*pattern of such abuse occurs in the area of products for individuals to*)
(*use, which is precisely where it is most unacceptable.Therefore, we*)
(*have designed this version of the GPL to prohibit the practice for those*)
(*products.If such problems arise substantially in other domains, we*)
(*stand ready to extend this provision to those domains in future versions*)
(*of the GPL, as needed to protect the freedom of users.*)
(**)
(*Finally, every program is threatened constantly by software patents.*)
(*States should not allow patents to restrict development and use of*)
(*software on general - purpose computers, but in those that do, we wish to*)
(*avoid the special danger that patents applied to a free program could*)
(*make it effectively proprietary.To prevent this, the GPL assures that*)
(*patents cannot be used to render the program non - free.*)
(**)
(*The precise terms and conditions for copying, distribution and*)
(*modification follow.*)
(**)
(*TERMS AND CONDITIONS*)
(**)
(*0. Definitions.*)
(**)
(*"This License" refers to version 3 of the GNU General Public License.*)
(**)
(*"Copyright" also means copyright - like laws that apply to other kinds of*)
(*works, such as semiconductor masks.*)
(**)
(*"The Program" refers to any copyrightable work licensed under this*)
(*License.Each licensee is addressed as "you"."Licensees" and*)
(*"recipients" may be individuals or organizations.*)
(**)
(*To "modify" a work means to copy from or adapt all or part of the work*)
(*in a fashion requiring copyright permission, other than the making of an*)
(*exact copy.The resulting work is called a "modified version" of the*)
(*earlier work or a work "based on" the earlier work.*)
(**)
(*A "covered work" means either the unmodified Program or a work based*)
(*on the Program.*)
(**)
(*To "propagate" a work means to do anything with it that, without*)
(*permission, would make you directly or secondarily liable for*)
(*infringement under applicable copyright law, except executing it on a*)
(*computer or modifying a private copy.Propagation includes copying,*)
(*distribution (with or without modification), making available to the*)
(*public, and in some countries other activities as well.*)
(**)
(*To "convey" a work means any kind of propagation that enables other*)
(*parties to make or receive copies.Mere interaction with a user through*)
(*a computer network, with no transfer of a copy, is not conveying.*)
(**)
(*An interactive user interface displays "Appropriate Legal Notices"*)
(*to the extent that it includes a convenient and prominently visible*)
(*feature that (1) displays an appropriate copyright notice, and (2)*)
(*tells the user that there is no warranty for the work (except to the*)
(*extent that warranties are provided), that licensees may convey the*)
(*work under this License, and how to view a copy of this License.If*)
(*the interface presents a list of user commands or options, such as a*)
(*menu, a prominent item in the list meets this criterion.*)
(**)
(*1. Source Code.*)
(**)
(*The "source code" for a work means the preferred form of the work*)
(*for making modifications to it."Object code" means any non - source*)
(*form of a work.*)
(**)
(*A "Standard Interface" means an interface that either is an official*)
(*standard defined by a recognized standards body, or, in the case of*)
(*interfaces specified for a particular programming language, one that*)
(*is widely used among developers working in that language.*)
(**)
(*The "System Libraries" of an executable work include anything, other*)
(*than the work as a whole, that (a) is included in the normal form of*)
(*packaging a Major Component, but which is not part of that Major*)
(*Component, and (b) serves only to enable use of the work with that*)
(*Major Component, or to implement a Standard Interface for which an*)
(*implementation is available to the public in source code form.A*)
(*"Major Component", in this context, means a major essential component*)
(*(kernel, window system, and so on) of the specific operating system*)
(*(if any) on which the executable work runs, or a compiler used to*)
(*produce the work, or an object code interpreter used to run it.*)
(**)
(*The "Corresponding Source" for a work in object code form means all*)
(*the source code needed to generate, install, and (for an executable*)
(*work) run the object code and to modify the work, including scripts to*)
(*control those activities.However, it does not include the work' s*)
(*System Libraries, or general - purpose tools or generally available free*)
(*programs which are used unmodified in performing those activities but*)
(*which are not part of the work.For example, Corresponding Source*)
(*includes interface definition files associated with source files for*)
(*the work, and the source code for shared libraries and dynamically*)
(*linked subprograms that the work is specifically designed to require,*)
(*such as by intimate data communication or control flow between those*)
(*subprograms and other parts of the work.*)
(**)
(*The Corresponding Source need not include anything that users*)
(*can regenerate automatically from other parts of the Corresponding*)
(*Source.*)
(**)
(*The Corresponding Source for a work in source code form is that*)
(*same work.*)
(**)
(*2. Basic Permissions.*)
(**)
(*All rights granted under this License are granted for the term of*)
(*copyright on the Program, and are irrevocable provided the stated*)
(*conditions are met.This License explicitly affirms your unlimited*)
(*permission to run the unmodified Program.The output from running a*)
(*covered work is covered by this License only if the output, given its*)
(*content, constitutes a covered work.This License acknowledges your*)
(*rights of fair use or other equivalent, as provided by copyright law.*)
(**)
(*You may make, run and propagate covered works that you do not*)
(*convey, without conditions so long as your license otherwise remains*)
(*in force.You may convey covered works to others for the sole purpose*)
(*of having them make modifications exclusively for you, or provide you*)
(*with facilities for running those works, provided that you comply with*)
(*the terms of this License in conveying all material for which you do*)
(*not control copyright.Those thus making or running the covered works*)
(*for you must do so exclusively on your behalf, under your direction*)
(*and control, on terms that prohibit them from making any copies of*)
(*your copyrighted material outside their relationship with you.*)
(**)
(*Conveying under any other circumstances is permitted solely under*)
(*the conditions stated below.Sublicensing is not allowed; section 10*)
(*makes it unnecessary.*)
(**)
(*3. Protecting Users' Legal Rights From Anti - Circumvention Law.*)
(**)
(*No covered work shall be deemed part of an effective technological*)
(*measure under any applicable law fulfilling obligations under article*)
(*11 of the WIPO copyright treaty adopted on 20 December 1996, or*)
(*similar laws prohibiting or restricting circumvention of such*)
(*measures.*)
(**)
(*When you convey a covered work, you waive any legal power to forbid*)
(*circumvention of technological measures to the extent such circumvention*)
(*is effected by exercising rights under this License with respect to*)
(*the covered work, and you disclaim any intention to limit operation or*)
(*modification of the work as a means of enforcing, against the work' s*)
(*users, your or third parties' legal rights to forbid circumvention of*)
(*technological measures.*)
(**)
(*4. Conveying Verbatim Copies.*)
(**)
(*You may convey verbatim copies of the Program' s source code as you*)
(*receive it, in any medium, provided that you conspicuously and*)
(*appropriately publish on each copy an appropriate copyright notice;*)
(*keep intact all notices stating that this License and any*)
(*non - permissive terms added in accord with section 7 apply to the code;*)
(*keep intact all notices of the absence of any warranty; and give all*)
(*recipients a copy of this License along with the Program.*)
(**)
(*You may charge any price or no price for each copy that you convey,*)
(*and you may offer support or warranty protection for a fee.*)
(**)
(*5. Conveying Modified Source Versions.*)
(**)
(*You may convey a work based on the Program, or the modifications to*)
(*produce it from the Program, in the form of source code under the*)
(*terms of section 4, provided that you also meet all of these conditions :*)
(**)
(*a) The work must carry prominent notices stating that you modified*)
(*it, and giving a relevant date.*)
(**)
(*b) The work must carry prominent notices stating that it is*)
(*released under this License and any conditions added under section*)
(*7. This requirement modifies the requirement in section 4 to*)
(*"keep intact all notices".*)
(**)
(*c) You must license the entire work, as a whole, under this*)
(*License to anyone who comes into possession of a copy.This*)
(*License will therefore apply, along with any applicable section 7*)
(*additional terms, to the whole of the work, and all its parts,*)
(*regardless of how they are packaged.This License gives no*)
(*permission to license the work in any other way, but it does not*)
(*invalidate such permission if you have separately received it.*)
(**)
(*d) If the work has interactive user interfaces, each must display*)
(*Appropriate Legal Notices; however, if the Program has interactive*)
(*interfaces that do not display Appropriate Legal Notices, your*)
(*work need not make them do so.*)
(**)
(*A compilation of a covered work with other separate and independent*)
(*works, which are not by their nature extensions of the covered work,*)
(*and which are not combined with it such as to form a larger program,*)
(*in or on a volume of a storage or distribution medium, is called an*)
(*"aggregate" if the compilation and its resulting copyright are not*)
(*used to limit the access or legal rights of the compilation' s users*)
(*beyond what the individual works permit.Inclusion of a covered work*)
(*in an aggregate does not cause this License to apply to the other*)
(*parts of the aggregate.*)
(**)
(*6. Conveying Non - Source Forms.*)
(**)
(*You may convey a covered work in object code form under the terms*)
(*of sections 4 and 5, provided that you also convey the*)
(*machine - readable Corresponding Source under the terms of this License,*)
(*in one of these ways :*)
(**)
(*a) Convey the object code in, or embodied in, a physical product*)
(*(including a physical distribution medium), accompanied by the*)
(*Corresponding Source fixed on a durable physical medium*)
(*customarily used for software interchange.*)
(**)
(*b) Convey the object code in, or embodied in, a physical product*)
(*(including a physical distribution medium), accompanied by a*)
(*written offer, valid for at least three years and valid for as*)
(*long as you offer spare parts or customer support for that product*)
(*model, to give anyone who possesses the object code either (1) a*)
(*copy of the Corresponding Source for all the software in the*)
(*product that is covered by this License, on a durable physical*)
(*medium customarily used for software interchange, for a price no*)
(*more than your reasonable cost of physically performing this*)
(*conveying of source, or (2) access to copy the*)
(*Corresponding Source from a network server at no charge.*)
(**)
(*c) Convey individual copies of the object code with a copy of the*)
(*written offer to provide the Corresponding Source.This*)
(*alternative is allowed only occasionally and noncommercially, and*)
(*only if you received the object code with such an offer, in accord*)
(*with subsection 6 b.*)
(**)
(*d) Convey the object code by offering access from a designated*)
(*place (gratis or for a charge), and offer equivalent access to the*)
(*Corresponding Source in the same way through the same place at no*)
(*further charge.You need not require recipients to copy the*)
(*Corresponding Source along with the object code.If the place to*)
(*copy the object code is a network server, the Corresponding Source*)
(*may be on a different server (operated by you or a third party)*)
(*that supports equivalent copying facilities, provided you maintain*)
(*clear directions next to the object code saying where to find the*)
(*Corresponding Source.Regardless of what server hosts the*)
(*Corresponding Source, you remain obligated to ensure that it is*)
(*available for as long as needed to satisfy these requirements.*)
(**)
(*e) Convey the object code using peer - to - peer transmission, provided*)
(*you inform other peers where the object code and Corresponding*)
(*Source of the work are being offered to the general public at no*)
(*charge under subsection 6 d.*)
(**)
(*A separable portion of the object code, whose source code is excluded*)
(*from the Corresponding Source as a System Library, need not be*)
(*included in conveying the object code work.*)
(**)
(*A "User Product" is either (1) a "consumer product", which means any*)
(*tangible personal property which is normally used for personal, family,*)
(*or household purposes, or (2) anything designed or sold for incorporation*)
(*into a dwelling.In determining whether a product is a consumer product,*)
(*doubtful cases shall be resolved in favor of coverage.For a particular*)
(*product received by a particular user, "normally used" refers to a*)
(*typical or common use of that class of product, regardless of the status*)
(*of the particular user or of the way in which the particular user*)
(*actually uses, or expects or is expected to use, the product.A product*)
(*is a consumer product regardless of whether the product has substantial*)
(*commercial, industrial or non - consumer uses, unless such uses represent*)
(*the only significant mode of use of the product.*)
(**)
(*"Installation Information" for a User Product means any methods,*)
(*procedures, authorization keys, or other information required to install*)
(*and execute modified versions of a covered work in that User Product from*)
(*a modified version of its Corresponding Source.The information must*)
(*suffice to ensure that the continued functioning of the modified object*)
(*code is in no case prevented or interfered with solely because*)
(*modification has been made.*)
(**)
(*If you convey an object code work under this section in, or with, or*)
(*specifically for use in, a User Product, and the conveying occurs as*)
(*part of a transaction in which the right of possession and use of the*)
(*User Product is transferred to the recipient in perpetuity or for a*)
(*fixed term (regardless of how the transaction is characterized), the*)
(*Corresponding Source conveyed under this section must be accompanied*)
(*by the Installation Information.But this requirement does not apply*)
(*if neither you nor any third party retains the ability to install*)
(*modified object code on the User Product (for example, the work has*)
(* been installed in ROM).*)
(**)
(*The requirement to provide Installation Information does not include a*)
(*requirement to continue to provide support service, warranty, or updates*)
(*for a work that has been modified or installed by the recipient, or for*)
(*the User Product in which it has been modified or installed.Access to a*)
(*network may be denied when the modification itself materially and*)
(*adversely affects the operation of the network or violates the rules and*)
(*protocols for communication across the network.*)
(**)
(*Corresponding Source conveyed, and Installation Information provided,*)
(*in accord with this section must be in a format that is publicly*)
(*documented (and with an implementation available to the public in*)
(*source code form), and must require no special password or key for*)
(*unpacking, reading or copying.*)
(**)
(*7. Additional Terms.*)
(**)
(*"Additional permissions" are terms that supplement the terms of this*)
(*License by making exceptions from one or more of its conditions.*)
(*Additional permissions that are applicable to the entire Program shall*)
(*be treated as though they were included in this License, to the extent*)
(*that they are valid under applicable law.If additional permissions*)
(*apply only to part of the Program, that part may be used separately*)
(*under those permissions, but the entire Program remains governed by*)
(*this License without regard to the additional permissions.*)
(**)
(*When you convey a copy of a covered work, you may at your option*)
(*remove any additional permissions from that copy, or from any part of*)
(*it.(Additional permissions may be written to require their own*)
(* removal in certain cases when you modify the work.) You may place*)
(*additional permissions on material, added by you to a covered work,*)
(*for which you have or can give appropriate copyright permission.*)
(**)
(*Notwithstanding any other provision of this License, for material you*)
(*add to a covered work, you may (if authorized by the copyright holders of*)
(*that material) supplement the terms of this License with terms :*)
(**)
(*a) Disclaiming warranty or limiting liability differently from the*)
(*terms of sections 15 and 16 of this License; or*)
(**)
(*b) Requiring preservation of specified reasonable legal notices or*)
(*author attributions in that material or in the Appropriate Legal*)
(*Notices displayed by works containing it; or*)
(**)
(*c) Prohibiting misrepresentation of the origin of that material, or*)
(*requiring that modified versions of such material be marked in*)
(*reasonable ways as different from the original version; or*)
(**)
(*d) Limiting the use for publicity purposes of names of licensors or*)
(*authors of the material; or*)
(**)
(*e) Declining to grant rights under trademark law for use of some*)
(*trade names, trademarks, or service marks; or*)
(**)
(*f) Requiring indemnification of licensors and authors of that*)
(*material by anyone who conveys the material (or modified versions of*)
(* it) with contractual assumptions of liability to the recipient, for*)
(*any liability that these contractual assumptions directly impose on*)
(*those licensors and authors.*)
(* *)
(* All other non - permissive additional terms are considered "further*)
(*restrictions" within the meaning of section 10. If the Program as you*)
(*received it, or any part of it, contains a notice stating that it is*)
(*governed by this License along with a term that is a further*)
(*restriction, you may remove that term.If a license document contains*)
(*a further restriction but permits relicensing or conveying under this*)
(*License, you may add to a covered work material governed by the terms*)
(*of that license document, provided that the further restriction does*)
(*not survive such relicensing or conveying.*)
(**)
(*If you add terms to a covered work in accord with this section, you*)
(*must place, in the relevant source files, a statement of the*)
(*additional terms that apply to those files, or a notice indicating*)
(*where to find the applicable terms.*)
(**)
(*Additional terms, permissive or non - permissive, may be stated in the*)
(*form of a separately written license, or stated as exceptions;*)
(*the above requirements apply either way.*)
(**)
(*8. Termination.*)
(**)
(*You may not propagate or modify a covered work except as expressly*)
(*provided under this License.Any attempt otherwise to propagate or*)
(*modify it is void, and will automatically terminate your rights under*)
(*this License (including any patent licenses granted under the third*)
(*paragraph of section 11).*)
(**)
(*However, if you cease all violation of this License, then your*)
(*license from a particular copyright holder is reinstated (a)*)
(*provisionally, unless and until the copyright holder explicitly and*)
(*finally terminates your license, and (b) permanently, if the copyright*)
(*holder fails to notify you of the violation by some reasonable means*)
(*prior to 60 days after the cessation.*)
(**)
(*Moreover, your license from a particular copyright holder is*)
(*reinstated permanently if the copyright holder notifies you of the*)
(*violation by some reasonable means, this is the first time you have*)
(*received notice of violation of this License (for any work) from that*)
(*copyright holder, and you cure the violation prior to 30 days after*)
(*your receipt of the notice.*)
(**)
(*Termination of your rights under this section does not terminate the*)
(*licenses of parties who have received copies or rights from you under*)
(*this License.If your rights have been terminated and not permanently*)
(*reinstated, you do not qualify to receive new licenses for the same*)
(*material under section 10.*)
(**)
(*9. Acceptance Not Required for Having Copies.*)
(**)
(*You are not required to accept this License in order to receive or*)
(*run a copy of the Program.Ancillary propagation of a covered work*)
(*occurring solely as a consequence of using peer - to - peer transmission*)
(*to receive a copy likewise does not require acceptance.However,*)
(*nothing other than this License grants you permission to propagate or*)
(*modify any covered work.These actions infringe copyright if you do*)
(*not accept this License.Therefore, by modifying or propagating a*)
(*covered work, you indicate your acceptance of this License to do so.*)
(**)
(*10. Automatic Licensing of Downstream Recipients.*)
(**)
(*Each time you convey a covered work, the recipient automatically*)
(*receives a license from the original licensors, to run, modify and*)
(*propagate that work, subject to this License.You are not responsible*)
(*for enforcing compliance by third parties with this License.*)
(**)
(*An "entity transaction" is a transaction transferring control of an*)
(*organization, or substantially all assets of one, or subdividing an*)
(*organization, or merging organizations.If propagation of a covered*)
(*work results from an entity transaction, each party to that*)
(*transaction who receives a copy of the work also receives whatever*)
(*licenses to the work the party' s predecessor in interest had or could*)
(*give under the previous paragraph, plus a right to possession of the*)
(*Corresponding Source of the work from the predecessor in interest, if*)
(*the predecessor has it or can get it with reasonable efforts.*)
(**)
(*You may not impose any further restrictions on the exercise of the*)
(*rights granted or affirmed under this License.For example, you may*)
(*not impose a license fee, royalty, or other charge for exercise of*)
(*rights granted under this License, and you may not initiate litigation*)
(*(including a cross - claim or counterclaim in a lawsuit) alleging that*)
(*any patent claim is infringed by making, using, selling, offering for*)
(*sale, or importing the Program or any portion of it.*)
(**)
(*11. Patents.*)
(**)
(*A "contributor" is a copyright holder who authorizes use under this*)
(*License of the Program or a work on which the Program is based.The*)
(*work thus licensed is called the contributor' s "contributor version".*)
(**)
(*A contributor' s "essential patent claims" are all patent claims*)
(*owned or controlled by the contributor, whether already acquired or*)
(*hereafter acquired, that would be infringed by some manner, permitted*)
(*by this License, of making, using, or selling its contributor version,*)
(*but do not include claims that would be infringed only as a*)
(*consequence of further modification of the contributor version.For*)
(*purposes of this definition, "control" includes the right to grant*)
(*patent sublicenses in a manner consistent with the requirements of*)
(*this License.*)
(* *)
(* Each contributor grants you a non - exclusive, worldwide, royalty - free*)
(*patent license under the contributor' s essential patent claims, to*)
(*make, use, sell, offer for sale, import and otherwise run, modify and*)
(*propagate the contents of its contributor version.*)
(**)
(*In the following three paragraphs, a "patent license" is any express*)
(*agreement or commitment, however denominated, not to enforce a patent*)
(*(such as an express permission to practice a patent or covenant not to*)
(*sue for patent infringement).To "grant" such a patent license to a*)
(*party means to make such an agreement or commitment not to enforce a*)
(*patent against the party.*)
(**)
(*If you convey a covered work, knowingly relying on a patent license,*)
(*and the Corresponding Source of the work is not available for anyone*)
(*to copy, free of charge and under the terms of this License, through a*)
(*publicly available network server or other readily accessible means,*)
(*then you must either (1) cause the Corresponding Source to be so*)
(*available, or (2) arrange to deprive yourself of the benefit of the*)
(*patent license for this particular work, or (3) arrange, in a manner*)
(*consistent with the requirements of this License, to extend the patent*)
(*license to downstream recipients."Knowingly relying" means you have*)
(*actual knowledge that, but for the patent license, your conveying the*)
(*covered work in a country, or your recipient' s use of the covered work*)
(*in a country, would infringe one or more identifiable patents in that*)
(*country that you have reason to believe are valid.*)
(**)
(*If, pursuant to or in connection with a single transaction or*)
(*arrangement, you convey, or propagate by procuring conveyance of, a*)
(*covered work, and grant a patent license to some of the parties*)
(*receiving the covered work authorizing them to use, propagate, modify*)
(*or convey a specific copy of the covered work, then the patent license*)
(*you grant is automatically extended to all recipients of the covered*)
(*work and works based on it.*)
(**)
(*A patent license is "discriminatory" if it does not include within*)
(*the scope of its coverage, prohibits the exercise of, or is*)
(*conditioned on the non - exercise of one or more of the rights that are*)
(*specifically granted under this License.You may not convey a covered*)
(*work if you are a party to an arrangement with a third party that is*)
(*in the business of distributing software, under which you make payment*)
(*to the third party based on the extent of your activity of conveying*)
(*the work, and under which the third party grants, to any of the*)
(*parties who would receive the covered work from you, a discriminatory*)
(*patent license (a) in connection with copies of the covered work*)
(*conveyed by you (or copies made from those copies), or (b) primarily*)
(*for and in connection with specific products or compilations that*)
(*contain the covered work, unless you entered into that arrangement,*)
(*or that patent license was granted, prior to 28 March 2007.*)
(**)
(*Nothing in this License shall be construed as excluding or limiting*)
(*any implied license or other defenses to infringement that may*)
(*otherwise be available to you under applicable patent law.*)
(**)
(*12. No Surrender of Others' Freedom.*)
(**)
(*If conditions are imposed on you (whether by court order, agreement or*)
(* otherwise) that contradict the conditions of this License, they do not*)
(*excuse you from the conditions of this License.If you cannot convey a*)
(*covered work so as to satisfy simultaneously your obligations under this*)
(*License and any other pertinent obligations, then as a consequence you may*)
(*not convey it at all.For example, if you agree to terms that obligate you*)
(*to collect a royalty for further conveying from those to whom you convey*)
(*the Program, the only way you could satisfy both those terms and this*)
(*License would be to refrain entirely from conveying the Program.*)
(**)
(*13. Use with the GNU Affero General Public License.*)
(**)
(*Notwithstanding any other provision of this License, you have*)
(*permission to link or combine any covered work with a work licensed*)
(*under version 3 of the GNU Affero General Public License into a single*)
(*combined work, and to convey the resulting work.The terms of this*)
(*License will continue to apply to the part which is the covered work,*)
(*but the special requirements of the GNU Affero General Public License,*)
(*section 13, concerning interaction through a network will apply to the*)
(*combination as such.*)
(**)
(*14. Revised Versions of this License.*)
(**)
(*The Free Software Foundation may publish revised and/or new versions of*)
(*the GNU General Public License from time to time.Such new versions will*)
(*be similar in spirit to the present version, but may differ in detail to*)
(*address new problems or concerns.*)
(**)
(*Each version is given a distinguishing version number.If the*)
(*Program specifies that a certain numbered version of the GNU General*)
(*Public License "or any later version" applies to it, you have the*)
(*option of following the terms and conditions either of that numbered*)
(*version or of any later version published by the Free Software*)
(*Foundation.If the Program does not specify a version number of the*)
(*GNU General Public License, you may choose any version ever published*)
(*by the Free Software Foundation.*)
(**)
(*If the Program specifies that a proxy can decide which future*)
(*versions of the GNU General Public License can be used, that proxy' s*)
(*public statement of acceptance of a version permanently authorizes you*)
(*to choose that version for the Program.*)
(**)
(*Later license versions may give you additional or different*)
(*permissions.However, no additional obligations are imposed on any*)
(*author or copyright holder as a result of your choosing to follow a*)
(*later version.*)
(**)
(*15. Disclaimer of Warranty.*)
(**)
(*THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY*)
(*APPLICABLE LAW.EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT*)
(*HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY*)
(*OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,*)
(*THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR*)
(*PURPOSE.THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM*)
(*IS WITH YOU.SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF*)
(*ALL NECESSARY SERVICING, REPAIR OR CORRECTION.*)
(**)
(*16. Limitation of Liability.*)
(**)
(*IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING*)
(*WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS*)
(*THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY*)
(*GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE*)
(*USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF*)
(*DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD*)
(*PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),*)
(*EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF*)
(*SUCH DAMAGES.*)
(**)
(*17. Interpretation of Sections 15 and 16.*)
(**)
(*If the disclaimer of warranty and limitation of liability provided*)
(*above cannot be given local legal effect according to their terms,*)
(*reviewing courts shall apply local law that most closely approximates*)
(*an absolute waiver of all civil liability in connection with the*)
(*Program, unless a warranty or assumption of liability accompanies a*)
(*copy of the Program in return for a fee.*)
(**)
(*END OF TERMS AND CONDITIONS*)


(* ::Subsection::Closed:: *)
(*Version information*)


(* ::Text:: *)
(*26/06/2012*)
(*1.0*)


(* ::Subsection::Closed:: *)
(*Notation guide*)


(* ::Text:: *)
(*Pf: Probability of false alarm*)
(*Pd: Probability of detection*)
(*M: Number of samples*)
(*\[Gamma]: Instantaneous signal to noise ratio*)
(*\bar{\[Gamma]}: Average instantaneous signal to noise ratio*)
(*n: Number of nodes*)
(*m: Nakagami fading parameter*)
(*\[Lambda]: Decision threshold at the local node or fusion center (the context will make clear which)*)
(*k: Fusion center voting rule*)
(*\[Rho]: Average correlation coefficient between nodes*)


(* ::Section::Closed:: *)
(*Q-function*)


Q::usage="Q[x] calculates the value of Gaussian Q-function at x.";
Q[x_]:=1-CDF[NormalDistribution[], x]


InverseQ::usage="InverseQ[P] calculates the value of the inverse of the Gaussian Q-function at P.";
InverseQ[P_]:=InverseCDF[NormalDistribution[], 1 - P]


(* ::Section:: *)
(*AWGN channel functions*)


(* ::Subsection::Closed:: *)
(*Decision probabilities*)


Options[AWGNProbabilityOfFalseAlarm]={Method->"Approximate"};
AWGNProbabilityOfFalseAlarm::usage="AWGNProbabilityOfFalseAlarm[M, \[Lambda]] calculates the probability of false alarm for a single energy detector operating on an AWGN channel.
AWGNProbabilityOfFalseAlarm[M, \[Lambda], n] calculates the probability of false alarm for a cooperative network operating on an AWGN channel.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[AWGNProbabilityOfFalseAlarm]]<>"\".";
AWGNProbabilityOfFalseAlarm[M_,\[Lambda]_,OptionsPattern[]]:=Module[{n = 1},AWGNProbabilityOfFalseAlarm[M,\[Lambda],n,Method->OptionValue[Method]]]
AWGNProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,OptionsPattern[]]:=Switch[OptionValue[Method],
	"Approximate",
	Q[(\[Lambda]-M n) / Sqrt[2M n]],
	"Exact",
	GammaRegularized[M n/2, \[Lambda] / 2],
	_,
	AWGNProbabilityOfFalseAlarm[M,\[Lambda],n]
]


Options[AWGNProbabilityOfDetection]={Method->"Approximate",LowSNR->True};
AWGNProbabilityOfDetection::usage="AWGNProbabilityOfDetection[M, \[Gamma], \[Lambda]] calculates the approximate probability of detection for a single energy detector operating on an AWGN channel.
AWGNProbabilityOfDetection[M, \[Gamma], \[Lambda], n] calculates the approximate probability of detection for a cooperative network operating on an AWGN channel.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[AWGNProbabilityOfDetection]]<>"\".

In addition, the LowSNR option can be used to specify whether to use a low signal to noise ratio approximation. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[AWGNProbabilityOfDetection]]<>".";
AWGNProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{n = 1},AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],n,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR]]]
AWGNProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,OptionsPattern[]]:=Switch[OptionValue[Method],
	"Approximate",
	If[OptionValue[LowSNR],
		Q[(\[Lambda]-M n(1+\[Gamma])) / Sqrt[2M n]],
		Q[(\[Lambda]-M n(1+\[Gamma])) / Sqrt[2M n(1+2\[Gamma])]]
	],
	"Exact",
	MarcumQ[M n / 2, Sqrt[M n \[Gamma]], Sqrt[\[Lambda]]],
	_,
	AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],n]
]


Options[\[Lambda]]={Method->"Approximate"};
\[Lambda]::usage="\[Lambda][M, Pf] calculates a threshold suitable for use in the calculation of the decision probabilities for a single energy detector.
\[Lambda][M, Pf, n] calculates a threshold suitable for use in the calculation of the fusion center decision probabilities when Nb = \[Infinity].

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[\[Lambda]]]<>"\".";
\[Lambda][M_,Pf_,OptionsPattern[]]:=Module[{n = 1},\[Lambda][M,Pf,n,Method->OptionValue[Method]]]
\[Lambda][M_,Pf_,n_,OptionsPattern[]]:=Switch[OptionValue[Method],
	"Approximate",
	Sqrt[2M n] InverseQ[Pf] + M n,
	"Exact",
	2 InverseGammaRegularized[M n / 2, Pf],
	_,
	\[Lambda][M,Pf,n]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


AWGNSampleComplexity::usage="AWGNSampleComplexity[\[Gamma], Pf, Pd] calculates the approximate number of samples required for a single energy detector to operate with the specified decision probabilities at a given signal to noise ratio in an AWGN channel.
AWGNSampleComplexity[\[Gamma], Pf, Pd, n] calculates the approximate number of samples required for a cooperative network to operate with the specified decision probabilities at a given signal to noise ratio in an AWGN channel.";
AWGNSampleComplexity[\[Gamma]_,Pf_,Pd_,n_:1]:= (2 / n) * ((InverseQ[Pf] - InverseQ[Pd]) / \[Gamma])^2


(* ::Section:: *)
(*Nakagami channel functions*)


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


Options[NakagamiPDF] = {Method->"Exact"};
NakagamiPDF::usage="NakagamiPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x] evaluates the probability density function of the instantaneous signal to noise ratio at a single energy detector operating on a Nakagami-m fading channel at x.
NakagamiPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x, n] evaluates the probability density function of the average instantaneous signal to noise ratio at the fusion center of a cooperative network operating on a Nakagami-m fading channel at x.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NakagamiPDF]]<>"\".";
NakagamiPDF[\[Gamma]_,m_,x_,OptionsPattern[]]:=Module[{n = 1},NakagamiPDF[\[Gamma],m,x,n,Method->OptionValue[Method]]]
NakagamiPDF[\[Gamma]_,m_,x_,n_,OptionsPattern[]]:=Switch[OptionValue[Method],
	"Exact",
	PDF[GammaDistribution[m n,\[Gamma]/m],x],
	"Approximate",
	PDF[NormalDistribution[n \[Gamma],Sqrt[n \[Gamma]^2 / m]],x],
	_,
	NakagamiPDF[\[Gamma],m,x,n,Method->"Exact"]
]


(* ::Subsection::Closed:: *)
(*Detection probability*)


Options[NNakagamiProbabilityOfDetection]={Method->"Approximate",Algorithm->"NGaussian",LowSNR->True,Timed->False,MaxIterations->1000,MaxTime->600,DatabaseLookup->False,DatabaseCaching->False};
NNakagamiProbabilityOfDetection::usage="NNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the probability of detection for a single energy detector operating on a Nakagami-m fading channel.
NNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the probability of detection for the fusion center of a cooperative network operating on a Nakagami-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NNakagamiProbabilityOfDetection]]<>"\".

For a given method, an algorithm must be specified. For the approximate method, the following algorithms may be specified:

Algorithm\[Rule]\"Gaussian\"
Algorithm\[Rule]\"NGaussian\"

By default, Algorithm\[Rule]\""<>ToString[Algorithm/.Options[NNakagamiProbabilityOfDetection]]<>"\". If Algorithm\[Rule]\"NGaussian\", then the LowSNR option may also be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NNakagamiProbabilityOfDetection]]<>".

For the exact method, the following algorithms may be specified:

Algorithm\[Rule]\"Annamalai\"
Algorithm\[Rule]\"Digham\"
Algorithm\[Rule]\"Herath\"

By default, Algorithm\[Rule]\"Annamalai\".

In addition, timing and database lookup/caching options may be (exclusively) specified. The timing option is specified by:

Timed\[Rule]"<>ToString[Timed/.Options[NNakagamiProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[NNakagamiProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[NNakagamiProbabilityOfDetection]]<>"

where the above options are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.

If timing is not used, then database lookup/caching may be enabled. This requires that the sqlite.m package be loaded. By default:

DatabaseLookup\[Rule]"<>ToString[DatabaseLookup/.Options[NNakagamiProbabilityOfDetection]]<>"
DatabaseCaching\[Rule]"<>ToString[DatabaseCaching/.Options[NNakagamiProbabilityOfDetection]]<>"

and the data is stored in the database specified in sqlite.m.
";
NNakagamiProbabilityOfDetection::opt="`1` and `2` options are mutually exclusive. Aborting...";
NNakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,OptionsPattern[]]:=Module[{n = 1},NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->OptionValue[Method], Algorithm->OptionValue[Algorithm], LowSNR->OptionValue[LowSNR], Timed->OptionValue[Timed], MaxIterations->OptionValue[MaxIterations], MaxTime->OptionValue[MaxTime], DatabaseLookup->OptionValue[DatabaseLookup], DatabaseCaching->OptionValue[DatabaseCaching]]]
NNakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,n_,OptionsPattern[]]:=Module[{f, lim, result, time = 0, totaltime = 0, iterations = 0, channelType = "Nakagami", rationalPf},
	If[OptionValue[DatabaseLookup]&&OptionValue[Timed],
		Message[NNakagamiProbabilityOfDetection::opt,"DatabaseLookup","Timed"];
		Abort[];
	];
	If[OptionValue[DatabaseCaching]&&OptionValue[Timed],
		Message[NNakagamiProbabilityOfDetection::opt,"DatabaseCaching","Timed"];
		Abort[];
	];
	lim = Switch[OptionValue[Algorithm],
		"Herath",
		HerathLimit[M,\[Gamma],\[Lambda],m,n],
		"Annamalai",
		AnnamalaiLimit[M,\[Gamma],\[Lambda],m,n],
		_,
		Null
	];
	f := Switch[OptionValue[Method],
		"Exact",
		Switch[OptionValue[Algorithm],
			"Digham",
			NDighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n],
			"Herath",
			NHerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,lim],
			"Annamalai",
			NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,lim],
			_,
			NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->"Exact",Algorithm->"Annamalai"]
		],
		"Approximate",
		Switch[OptionValue[Algorithm],
			"Gaussian",
			NHorganNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n],
			"NGaussian",
			NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n, LowSNR->OptionValue[LowSNR]],
			_,
			NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->"Approximate",Algorithm->"NGaussian"]
		],
		_,
		NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n]
	];
	If[OptionValue[DatabaseLookup],
		result = GetResult[OptionValue[Algorithm],channelType,M,\[Gamma],ProbabilityOfFalseAlarm[M,\[Lambda],n,Method->OptionValue[Method]]//N,n,m];
		If[TrueQ[result==Null],
			result = f;
			If[OptionValue[DatabaseCaching],
				(* For correct retrieval of results later, attempt to convert Pf to rational form *)
				rationalPf = Round[ProbabilityOfFalseAlarm[M,\[Lambda],n,Method->"Exact"]*10^6//N]/10^6;
				CacheResult[OptionValue[Algorithm],channelType,M,\[Gamma],rationalPf,n,m,result//N,OptionValue[Algorithm]];
			];
		];
		result,
		If[OptionValue[Timed],
			(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
			While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
				ClearSystemCache[];
				{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
				totaltime += time;
				iterations++;
			];
			{result,totaltime/iterations},
			f
		]
	]
]


NDighamNakagamiProbabilityOfDetection::usage="NDighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Digham's algorithm.
NDighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Digham's algorithm.";
NDighamNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_:1]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		Module[{A1, \[Beta]},
			A1 = Exp[-\[Lambda] \[Beta] / (2 m n)] (\[Beta]^(m n - 1) LaguerreL[m n - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m n - 2}]]);
			\[Beta] = (2m) / (2m + M \[Gamma]);
			A1 + \[Beta]^(m n) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m n, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M n / 2) - 1}]]
		],
	20]
]


Options[HerathLimit]={Tolerance->10^-6};
HerathLimit::usage="HerathLimit[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in Herath's algorithm using the default tolerance for a single energy detector.
HerathLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Herath's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[HerathLimit]//N//InputForm]<>".";
HerathLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{x0,x1,f,fx0,tol=OptionValue[Tolerance]},
	Block[{$MaxExtraPrecision=\[Infinity]},
		f[j_?NumericQ]:=N[(m / ((M / 2) \[Gamma] + m))^(n m) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]),20];
		(* Use Newton's method to find root *)
		Ceiling[j/.FindRoot[f[j]==tol,{j,Ceiling[M n/2]}]]
	]
]


NHerathNakagamiProbabilityOfDetection::usage="NHerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, lim] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Herath's algorithm and truncation point lim.
NHerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n, lim] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Herath's algorithm and truncation point lim.";
NHerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,lim_?IntegerQ]:=Module[{n = 1},HerathLimit[M,\[Gamma],\[Lambda],m,n,lim]]
NHerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,lim_?IntegerQ]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M n/ 2), lim}]],
	20]
]


Options[AnnamalaiLimit]={Tolerance->10^-8};
AnnamalaiLimit::usage="AnnamalaiLimit[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in Herath's algorithm using the default tolerance for a single energy detector.
AnnamalaiLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Herath's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[AnnamalaiLimit]//N//InputForm]<>".";
AnnamalaiLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{tol=OptionValue[Tolerance]},
	1/2 (2(\[Lambda] / Sqrt[2M n] - InverseCDF[NormalDistribution[],tol])^2 - M n)//N//Ceiling
]


NAnnamalaiNakagamiProbabilityOfDetection::usage="NAnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, lim] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Annamalai's algorithm and truncation point lim.
NAnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n, lim] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Annamalai's algorithm and truncation point lim.";
NAnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,lim_?IntegerQ]:=Module[{n = 1},NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,lim]]
NAnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,lim_?IntegerQ]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		1 - (2m / (2m + M \[Gamma]))^(m n) (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) ((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k (1-GammaRegularized[M n/2+k, \[Lambda] / 2]),{k,1,lim}]],
	20]
]


<<erf_approx.m;
Options[NHorganNakagamiProbabilityOfDetection]={LargeMN->10};
NHorganNakagamiProbabilityOfDetection::usage="NHorganNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using Horgan's algorithm
NHorganNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using Horgan's algorithm";
NHorganNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n=1},NHorganNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,LargeMN->OptionValue[LargeMN]]]
NHorganNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		Module[{x = Round[m n], tol = 10^-6},
			(* This method can only be used when m * n is an integer *)
			If[Abs[m n - x] <= tol,
				If[x < OptionValue[LargeMN],
					AWGNProbabilityOfFalseAlarm[M,\[Lambda],n] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2 n) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M n) / (M (\[Gamma] / m))) * t]Erfc[(2n t - (\[Lambda] - M n) * (\[Gamma] / m)) / (2Sqrt[M n] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),
					(1/2 (1+Erf[(m (M n (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma])])+1/2 E^((4 c M n (-m+a M \[Gamma]^2)+b (-b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))-2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (-M n (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]-Erf[(-2 m n+\[Gamma] (-2 a M n+Sqrt[2] b Sqrt[M n]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[n (m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M n (m-a M \[Gamma]^2)+b (b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))+2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (M n (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]))/.LopezBenitezParameters[(-M (n+\[Gamma])+\[Lambda])/(2 Sqrt[M n])]
				],
				Undefined
			]
		],
	20]
]


Options[NGaussianNakagamiProbabilityOfDetection] = {LowSNR->True};
NGaussianNakagamiProbabilityOfDetection::usage="NGaussianNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using a numerical algorithm
NGaussianNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using a numerical algorithm.

In addition, the LowSNR option may be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NGaussianNakagamiProbabilityOfDetection]]<>".";
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,LowSNR->OptionValue[LowSNR]]]
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n, Method->"Approximate", LowSNR->OptionValue[LowSNR]]NakagamiPDF[\[Gamma],m,x,n],{x,0,\[Infinity]}],
	20]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Options[NNakagamiSampleComplexity]={Method->"Approximate", LowSNR->True, Tolerance->10^-6};
NNakagamiSampleComplexity::usage="NNakagamiSampleComplexity[\[Gamma], Pf, Pd, m] calculates the sample complexity for a single energy detector operating on a Nakagami-m fading channel.
NNakagamiSampleComplexity[\[Gamma], Pf, Pd, m, n] calculates the sample complexity for a cooperative network operating on a Nakagami-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NNakagamiSampleComplexity]]<>"\".

If Method\[Rule]\"Approximate\", the LowSNR option may be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NNakagamiSampleComplexity]]<>".

Numerical tolerance can be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[NNakagamiSampleComplexity]//N//InputForm]<>".";
NNakagamiSampleComplexity::tol="The difference between the result `1` and the constraint `2` was greater than the specified tolerance `3`.";
NNakagamiSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,Method->OptionValue[Method], LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]
NNakagamiSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{tol = OptionValue[Tolerance], intialGuess = Max[(20 / (n m^2)), 1] * SampleComplexity[\[Gamma],Pf,Pd,n], courseGuess, fineGuess, result},
	(* Temporarily disable error checking - we'll do our own *)
	Off[FindRoot::reged,FindRoot::lstol];
	Switch[OptionValue[Method],
		"Approximate",
		(* Only use Gaussian method if it is valid *)
		If[intialGuess <= 250,
			result = NNakagamiSampleComplexity[\[Gamma],Pf,Pd,n,m,tol,Method->"Exact"];,
			fineGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,Method->"NGaussian",LowSNR->OptionValue[LowSNR]] == Pd, {M, intialGuess, 1, \[Infinity]}];
			result = NNakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n],m,n,Method->"NGaussian",LowSNR->OptionValue[LowSNR]];
		];,
		"Exact",
		(* If Gaussian approximation is valid, then use it to speed up the calculation *)
		If[intialGuess <= 250,
			courseGuess = intialGuess;,
			courseGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,Method->"NGaussian"] == Pd, {M, intialGuess, 1, \[Infinity]}];
		];
		fineGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[courseGuess,\[Gamma],\[Lambda][courseGuess,Pf,n,Method->"Exact"],m,n] == Pd, {M, courseGuess, 1, \[Infinity]}];
		result = NNakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n,Method->"Exact"],m,n];
	];
	On[FindRoot::reged,FindRoot::lstol];
	If[Abs[result - Pd] <= tol//TrueQ,
		fineGuess,
		Message[NNakagamiSampleComplexity::tol, result//N, Pd//N, tol//N]
	]
]


(* ::Section:: *)
(*Rayleigh channel functions*)


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


Options[RayleighPDF] = Options[NakagamiPDF];
RayleighPDF::usage=StringReplace[NakagamiPDF::usage,{"Nakagami-m"->"Rayleigh","Nakagami"->"Rayleigh",", m"->""}];
RayleighPDF[\[Gamma]_,x_,OptionsPattern[]]:=Module[{n = 1},RayleighPDF[\[Gamma],x,n,Method->OptionValue[Method]]]
RayleighPDF[\[Gamma]_,x_,n_,OptionsPattern[]]:=Module[{m = 1},NakagamiPDF[\[Gamma],m,x,n,Method->OptionValue[Method]]]


(* ::Subsection::Closed:: *)
(*Detection probability*)


Options[NRayleighProbabilityOfDetection]=Options[NNakagamiProbabilityOfDetection];
NRayleighProbabilityOfDetection::usage=StringReplace[NNakagamiProbabilityOfDetection::usage,{"Nakagami-m"->"Rayleigh","Nakagami"->"Rayleigh",", m"->""}];
NRayleighProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,OptionsPattern[]]:=Module[{n = 1},NRayleighProbabilityOfDetection[M,\[Gamma],\[Lambda],n,Method->OptionValue[Method], Algorithm->OptionValue[Algorithm], LowSNR->OptionValue[LowSNR], Timed->OptionValue[Timed], MaxIterations->OptionValue[MaxIterations], MaxTime->OptionValue[MaxTime], DatabaseLookup->OptionValue[DatabaseLookup], DatabaseCaching->OptionValue[DatabaseCaching]]]
NRayleighProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,n_?IntegerQ,OptionsPattern[]]:=Module[{m = 1},NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->OptionValue[Method], Algorithm->OptionValue[Algorithm], LowSNR->OptionValue[LowSNR], Timed->OptionValue[Timed], MaxIterations->OptionValue[MaxIterations], MaxTime->OptionValue[MaxTime], DatabaseLookup->OptionValue[DatabaseLookup], DatabaseCaching->OptionValue[DatabaseCaching]]]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Options[NRayleighSampleComplexity]=Options[NNakagamiSampleComplexity];
NRayleighSampleComplexity::usage=StringReplace[NNakagamiSampleComplexity::usage,{"Nakagami-m"->"Rayleigh","Nakagami"->"Rayleigh",", m"->""}];
NRayleighSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,OptionsPattern[]]:=Module[{n = 1},NRayleighSampleComplexity[\[Gamma],Pf,Pd,n,Method->OptionValue[Method], LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]
NRayleighSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{m = 1},NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,Method->OptionValue[Method], LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]


(* ::Section:: *)
(*Network / fusion center functions*)


(* ::Subsection::Closed:: *)
(*Decision probabilities*)


Options[ProbabilityOfFalseAlarm] = {DecisionBits->\[Infinity],CorrelationCoefficient->0,Method->"Approximate"};
ProbabilityOfFalseAlarm::usage="ProbabilityOfFalseAlarm[M, \[Lambda]] calculates the probability of false alarm for a single energy detector.
ProbabilityOfFalseAlarm[M, \[Lambda], n] calculates the probability of false alarm for a cooperative network of energy detectors with infinite precision decision fusion.
ProbabilityOfFalseAlarm[M, \[Lambda], n, k] calculates the probability of false alarm for a cooperative network of energy detectors with 1 bit decision fusion.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[ProbabilityOfFalseAlarm]]<>"\".

Additionally, the number of decision bits used in fusion may be specified by the DecisionBits option. By default, DecisionBits\[Rule]"<>ToString[DecisionBits/.Options[ProbabilityOfFalseAlarm]]<>". DecisionBits \[Element] {1,\[Infinity]}.

If 1 bit decision fusion is specified, then the average correlation between nodes may be specified with the CorrelationCoefficient option. By default, CorrelationCoefficient\[Rule]"<>ToString[CorrelationCoefficient/.Options[ProbabilityOfFalseAlarm]]<>".";
ProbabilityOfFalseAlarm[M_,\[Lambda]_,OptionsPattern[]]:=Module[{n=1},ProbabilityOfFalseAlarm[M,\[Lambda],n,DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{k=Null},ProbabilityOfFalseAlarm[M,\[Lambda],n,k,DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,k_,OptionsPattern[]]:=If[OptionValue[DecisionBits]==\[Infinity],
	If[OptionValue[CorrelationCoefficient]==0,
		AWGNProbabilityOfFalseAlarm[M,\[Lambda],n,Method->OptionValue[Method]],
		Undefined
	],
	Module[{Pf},
		Pf = AWGNProbabilityOfFalseAlarm[M,\[Lambda],Method->OptionValue[Method]];
		FusionCenterProbabilityOfFalseAlarm[Pf,n,k,OptionValue[CorrelationCoefficient]]
	]
]


Options[ProbabilityOfDetection] = {ChannelType->"AWGN",DecisionBits->\[Infinity],CorrelationCoefficient->0,Method->"Approximate"};
ProbabilityOfDetection::usage="ProbabilityOfDetection[M, \[Gamma], \[Lambda]] calculates the probability of detection for a single energy detector for the specified channel type.
ProbabilityOfDetection[M, \[Gamma], \[Lambda], n] calculates the probability of detection for a cooperative network of energy detectors with infinite precision decision fusion for the specified channel type.
ProbabilityOfDetection[M, \[Gamma], \[Lambda], n, k] calculates the probability of detection for a cooperative network of energy detectors with 1 bit decision fusion for the specified channel type.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[ProbabilityOfDetection]]<>"\".

The following channel types may be specified:

ChannelType\[Rule]\"AWGN\"
ChannelType\[Rule]\"Rayleigh\"
ChannelType\[Rule]{\"Nakagami\",m}

By default, ChannelType\[Rule]\""<>ToString[ChannelType/.Options[ProbabilityOfDetection]]<>"\".

Additionally, the number of decision bits used in fusion may be specified by the DecisionBits option. By default, DecisionBits\[Rule]"<>ToString[DecisionBits/.Options[ProbabilityOfFalseAlarm]]<>". DecisionBits \[Element] {1,\[Infinity]}.

If 1 bit decision fusion is specified, then the average correlation between nodes may be specified with the CorrelationCoefficient option. By default, CorrelationCoefficient\[Rule]"<>ToString[CorrelationCoefficient/.Options[ProbabilityOfDetection]]<>".";
ProbabilityOfDetection::k="Error: must specify a voting rule when DecisionBits\[Rule]1";
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{n=1},ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,ChannelType->OptionValue[ChannelType],DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{k=Null},ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,ChannelType->OptionValue[ChannelType],DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,k_,OptionsPattern[]]:=Module[{channelType,m,\[Rho]=OptionValue[CorrelationCoefficient]},
	If[ListQ[OptionValue[ChannelType]],
		{channelType,m} = OptionValue[ChannelType];,
		channelType = OptionValue[ChannelType];
	];
	If[OptionValue[DecisionBits]==\[Infinity],
		If[\[Rho]==0,
			Switch[channelType,
				"AWGN",
				AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],n,Method->OptionValue[Method]],
				"Rayleigh",
				NRayleighProbabilityOfDetection[M,\[Gamma],\[Lambda],n,Method->OptionValue[Method]],
				"Nakagami",
				NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->OptionValue[Method]],
				_,
				ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,ChannelType->"AWGN",CorrelationCoefficient->\[Rho],Method->OptionValue[Method]]
			],
			Undefined
		],
		If[k==Null,
			Message[ProbabilityOfDetection::k];
			Abort[],
			Module[{Pd},
				Pd = Switch[channelType,
					"AWGN",
					AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],Method->OptionValue[Method]],
					"Rayleigh",
					NRayleighProbabilityOfDetection[M,\[Gamma],\[Lambda],Method->OptionValue[Method]],
					"Nakagami",
					NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,Method->OptionValue[Method]],
					_,
					ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,ChannelType->"AWGN",CorrelationCoefficient->\[Rho],Method->OptionValue[Method]]
				];
				FusionCenterProbabilityOfDetection[Pd,n,k,\[Rho]]
			]
		]
	]
]


FusionCenterProbabilityOfFalseAlarm::usage="FusionCenterProbabilityOfFalseAlarm[Pf, n, k] calculates the overall probability of false alarm for a cooperative network with 1 bit decision fusion.
FusionCenterProbabilityOfFalseAlarm[Pf, n, k, \[Rho]] calculates the overall probability of false alarm for a cooperative network with 1 bit decision fusion and correlated decisions.";
FusionCenterProbabilityOfFalseAlarm[Pf_?NumericQ,n_?NumericQ,k_?NumericQ,\[Rho]_:0]:=If[\[Rho] == 0,
	1 - CDF[BinomialDistribution[n, Pf], k - 1],
	Switch[n,
		1,
		Pf,
		2,
		Undefined,
		_,
		Sum[Sum[(-1)^i Binomial[l,i] Pf Product[(\[Rho](s + 1 - Pf) + Pf) / (1 + s \[Rho]),{s, 0, n - l + i - 2}],{i, 0, l}],{l, 0, k - 1}]
	]
]


FusionCenterProbabilityOfDetection::usage="FusionCenterProbabilityOfDetection[Pd, n, k] calculates the overall probability of detection for a cooperative network with 1 bit decision fusion.
FusionCenterProbabilityOfDetection[Pd, n, k, \[Rho]] calculates the overall probability of detection for a cooperative network with 1 bit decision fusion and correlated decisions.";
FusionCenterProbabilityOfDetection[Pd_?NumericQ,n_?NumericQ,k_?NumericQ,\[Rho]_:0]:=FusionCenterProbabilityOfFalseAlarm[Pd,n,k,\[Rho]]


k::usage="k[Pf, Pd, n] calculates the optimum fusion rule for a cooperative network with 1 bit decision fusion.
k[Pf, Pd, n, \[Rho]] calculates the optimum fusion rule for a cooperative network with 1 bit decision fusion and correlated decisions.";
k[Pf_?NumericQ,Pd_?NumericQ,n_?NumericQ,\[Rho]_:0]:=If[\[Rho] == 0,
	Min[n, \[LeftCeiling](n Log[(1 - Pd) / (1 - Pf)]) / Log[((1 - Pd) Pf) / (Pd (1 - Pf))]\[RightCeiling]],
	Module[{a = Table[FusionCenterProbabilityOfFalseAlarm[Pf,n,k,\[Rho]]+1-FusionCenterProbabilityOfDetection[Pd,n,k,\[Rho]],{k,1,n}]},
		Position[a, Min[a]][[1, 1]]
	]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Options[SampleComplexity]={ChannelType->"AWGN",DecisionBits->\[Infinity],CorrelationCoefficient->0,Method->OptionValue[NNakagamiSampleComplexity,Method],LowSNR->OptionValue[NNakagamiSampleComplexity,LowSNR],Tolerance->OptionValue[NNakagamiSampleComplexity,Tolerance]};
SampleComplexity::usage="SampleComplexity[\[Gamma], Pf, Pd] calculates the number of samples required for a single energy detector to operate with the specified decision probabilities at the given signal to noise ratio.
SampleComplexity[\[Gamma], Pf, Pd, n] calculates the number of samples required for a cooperative network of energy detectors to operate with the specified decision probabilities at the given signal to noise ratio.

The following methods may be specified for Rayleigh and Nakagami channels:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[SampleComplexity]]<>"\".

The following channel types may be specified:

ChannelType\[Rule]\"AWGN\"
ChannelType\[Rule]\"Rayleigh\"
ChannelType\[Rule]{\"Nakagami\",m}

By default, ChannelType\[Rule]\""<>ToString[ChannelType/.Options[SampleComplexity]]<>"\".

Additionally, the number of decision bits used in fusion may be specified by the DecisionBits option. By default, DecisionBits\[Rule]"<>ToString[DecisionBits/.Options[SampleComplexity]]<>". DecisionBits \[Element] {1,\[Infinity]}.

If 1 bit decision fusion is specified, then the average correlation between nodes may be specified with the CorrelationCoefficient option. By default, CorrelationCoefficient\[Rule]"<>ToString[CorrelationCoefficient/.Options[SampleComplexity]]<>".";
SampleComplexity::tol=NNakagamiSampleComplexity::tol;
SampleComplexity[\[Gamma]_,Pf_,Pd_,OptionsPattern[]]:=Module[{n=1},SampleComplexity[\[Gamma],Pf,Pd,n,ChannelType->OptionValue[ChannelType],DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]
SampleComplexity[\[Gamma]_,Pf_,Pd_,n_,OptionsPattern[]]:=Module[{channelType,m,\[Rho]=OptionValue[CorrelationCoefficient],x,y,tol=OptionValue[Tolerance]},
	If[ListQ[OptionValue[ChannelType]],
		{channelType,m} = OptionValue[ChannelType];,
		channelType = OptionValue[ChannelType];
	];
	If[OptionValue[DecisionBits]==\[Infinity],
		If[\[Rho]==0,
			Switch[channelType,
				"AWGN",
				AWGNSampleComplexity[\[Gamma],Pf,Pd,n],
				"Rayleigh",
				NRayleighSampleComplexity[\[Gamma],Pf,Pd,n,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				"Nakagami",
				NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				_,
				SampleComplexity[\[Gamma],Pf,Pd,n,k,ChannelType->"AWGN",DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->\[Rho],Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]
			],
			(* No solution for correlated infinite precision fusion *)
			Undefined
		],
		If[\[Rho]!=0&&n==2,
			(* No solution for correlated fusion when n = 2 *)
			Undefined,
			(* Temporarily disable checks - we'll do our own after *)
			Off[FindRoot::lstol,NMinimize::cvmit];
			{x,y} = {x,y}/.(NMinimize[{Abs[FusionCenterProbabilityOfDetection[x,n,k[y,x,n,\[Rho]],\[Rho]]-Pd]+Abs[FusionCenterProbabilityOfFalseAlarm[y,n,k[y,x,n,\[Rho]],\[Rho]]-Pf],0<x<1&&0<y<1},{x,y}][[2]]);
			On[FindRoot::lstol,NMinimize::cvmit];
			If[!(Abs[FusionCenterProbabilityOfDetection[x,n,k[y,x,n,\[Rho]],\[Rho]] - Pd] <= tol),Message[SampleComplexity::tol, FusionCenterProbabilityOfDetection[x,n,k[y,x,n,\[Rho]],\[Rho]]//N, Pd//N, tol//N]];
			If[!(Abs[FusionCenterProbabilityOfFalseAlarm[y,n,k[y,x,n,\[Rho]],\[Rho]] - Pf] <= tol),Message[SampleComplexity::tol, FusionCenterProbabilityOfFalseAlarm[y,n,k[y,x,n,\[Rho]],\[Rho]]//N, Pf//N, tol//N]];
			Switch[channelType,
				"AWGN",
				AWGNSampleComplexity[\[Gamma],y,x],
				"Rayleigh",
				NRayleighSampleComplexity[\[Gamma],y,x,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				"Nakagami",
				NNakagamiSampleComplexity[\[Gamma],y,x,m,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				_,
				SampleComplexity[\[Gamma],y,x,n,ChannelType->"AWGN",DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->\[Rho],Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]
			]
		]
	]
]


(* ::Section::Closed:: *)
(*Database connectivity*)


<<sqlite.m


GetResult::usage="GetResult[algorithm, channelType, M, \[Gamma], Pf, n, m] fetches the specified record from the database.";
GetResult[algorithm_?StringQ,channelType_?StringQ,M_?NumericQ,\[Gamma]_?NumericQ,Pf_?NumericQ,n_?IntegerQ,m_?NumericQ]:=Module[{databaseName="data.sqlite",tableName="data",columnNames={"algorithm","channelType","sampleComplexity","n","m","snrdb","pf","pd"},db,result},
	If[FileExistsQ[databaseName],
		db = SQLiteOpenDatabase[databaseName];
		result = SQLiteLookupRecord[db,tableName,columnNames,{algorithm,channelType,M,n,m,10Log[10,\[Gamma]]//Round,Pf//N,Null}];
		SQLiteCloseDatabase[db];,
		result = Undefined;
	];
	If[result == {}//TrueQ,
		Null,
		result[[1]][[1]]
	]
]


CacheResult::usage="CacheResult[algorithm, channelType, M, \[Gamma], Pf, n, m, result] caches the specified record in the database.";
CacheResult[algorithm_?StringQ,channelType_?StringQ,M_?NumericQ,\[Gamma]_?NumericQ,Pf_?NumericQ,n_?IntegerQ,m_?NumericQ,result_?NumericQ]:=Module[{databaseName="data.sqlite",tableName="data",columnNames={"algorithm","channelType","sampleComplexity","n","m","snrdb","pf","pd"},columnTypes={"TEXT","TEXT","NUMERIC","INTEGER","NUMERIC","INTEGER","NUMERIC","NUMERIC"},db},
	If[!FileExistsQ[databaseName],
		db = SQLiteOpenDatabase[databaseName];
		SQLiteCreateTable[db, tableName, columnNames, columnTypes],
		db = SQLiteOpenDatabase[databaseName];
	];
	SQLiteInsertRecord[db,tableName,columnNames,{algorithm,channelType,M,n,m,10Log[10,\[Gamma]]//Round,Pf//N,result,algorithm}];
	SQLiteCloseDatabase[db];
]
