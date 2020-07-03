module Fantomas.Tests.ListTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``array indices``() =
    formatSourceString false """
let array1 = [| 1; 2; 3 |]
array1.[0..2] 
array2.[2.., 0..]
array2.[..3, ..1] 
array1.[1] <- 3
    """ config
    |> prepend newline
    |> should equal """
let array1 = [| 1; 2; 3 |]
array1.[0..2]
array2.[2.., 0..]
array2.[..3, ..1]
array1.[1] <- 3
"""

[<Test>]
let ``array values``() =
    formatSourceString false """
let arr = [|(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)|]
    """ { config with SemicolonAtEndOfLine = true }
    |> prepend newline
    |> should equal """
let arr =
    [| (1, 1, 1);
       (1, 2, 2);
       (1, 3, 3);
       (2, 1, 2);
       (2, 2, 4);
       (2, 3, 6);
       (3, 1, 3);
       (3, 2, 6);
       (3, 3, 9) |]
"""

[<Test>]
let ``cons and list patterns``() =
    formatSourceString false """
let rec printList l =
    match l with
    | head :: tail -> printf "%d " head; printList tail
    | [] -> printfn ""

let listLength list =
    match list with
    | [] -> 0
    | [ _ ] -> 1
    | [ _; _ ] -> 2
    | [ _; _; _ ] -> 3
    | _ -> List.length list"""  config
    |> prepend newline
    |> should equal """
let rec printList l =
    match l with
    | head :: tail ->
        printf "%d " head
        printList tail
    | [] -> printfn ""

let listLength list =
    match list with
    | [] -> 0
    | [ _ ] -> 1
    | [ _; _ ] -> 2
    | [ _; _; _ ] -> 3
    | _ -> List.length list
"""

[<Test>]
let ``array patterns``() =
    formatSourceString false """
let vectorLength vec =
    match vec with
    | [| var1 |] -> var1
    | [| var1; var2 |] -> sqrt (var1*var1 + var2*var2)
    | [| var1; var2; var3 |] -> sqrt (var1*var1 + var2*var2 + var3*var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)""" config
    |> prepend newline
    |> should equal """
let vectorLength vec =
    match vec with
    | [| var1 |] -> var1
    | [| var1; var2 |] -> sqrt (var1 * var1 + var2 * var2)
    | [| var1; var2; var3 |] -> sqrt (var1 * var1 + var2 * var2 + var3 * var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)
"""

[<Test>]
let ``should keep -> notation``() =
    formatSourceString false """let environVars target =
    [for e in Environment.GetEnvironmentVariables target ->
        let e1 = e :?> Collections.DictionaryEntry
        e1.Key, e1.Value]
    """ config
    |> prepend newline
    |> should equal """
let environVars target =
    [ for e in Environment.GetEnvironmentVariables target ->
        let e1 = e :?> Collections.DictionaryEntry
        e1.Key, e1.Value ]
"""

[<Test>]
let ``list comprehensions``() =
    formatSourceString false """
let listOfSquares = [ for i in 1 .. 10 -> i*i ]
let list0to3 = [0 .. 3]""" config
    |> prepend newline
    |> should equal """
let listOfSquares = [ for i in 1 .. 10 -> i * i ]
let list0to3 = [ 0 .. 3 ]
"""

[<Test>]
let ``array comprehensions``() =
    formatSourceString false """
let a1 = [| for i in 1 .. 10 -> i * i |]
let a2 = [| 0 .. 99 |]  
let a3 = [| for n in 1 .. 100 do if isPrime n then yield n |]""" config
    |> prepend newline
    |> should equal """
let a1 = [| for i in 1 .. 10 -> i * i |]
let a2 = [| 0 .. 99 |]

let a3 =
    [| for n in 1 .. 100 do
        if isPrime n then yield n |]
"""

[<Test>]
let ``should keep Array2D``() =
    formatSourceString false """
let cast<'a> (A:obj[,]):'a[,] = A |> Array2D.map unbox
let flatten (A:'a[,]) = A |> Seq.cast<'a>
let getColumn c (A:_[,]) = flatten A.[*,c..c] |> Seq.toArray""" config
    |> prepend newline
    |> should equal """
let cast<'a> (A: obj [,]): 'a [,] = A |> Array2D.map unbox
let flatten (A: 'a [,]) = A |> Seq.cast<'a>
let getColumn c (A: _ [,]) = flatten A.[*, c..c] |> Seq.toArray
"""

[<Test>]
let ``should be able to support F# 3.1 slicing``() =
    formatSourceString false """
let x = matrix.[*, 3]
let y = matrix.[3, *]""" config
    |> prepend newline
    |> should equal """
let x = matrix.[*, 3]
let y = matrix.[3, *]
"""

[<Test>]
let ``comment after string in list`` () =
    formatSourceString false """let xxxxxxxxxxxx = ["yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" //
                    "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" //
                    "ffffffffffffffffffffffffffffffffffffffff"]
"""  config
    |> should equal """let xxxxxxxxxxxx =
    [ "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" //
      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" //
      "ffffffffffffffffffffffffffffffffffffffff" ]
"""

[<Test>]
let ``multiline list should print each item on newline`` () =
    formatSourceString false """let xxxxxxxxxxxx = ["yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"; "ddddddddddddddddddddddddddddddddddddddddd"
                    "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz";
                    "ffffffffffffffffffffffffffffffffffffffff"]
"""  config
    |> should equal """let xxxxxxxxxxxx =
    [ "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"
      "ddddddddddddddddddddddddddddddddddddddddd"
      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
      "ffffffffffffffffffffffffffffffffffffffff" ]
"""

[<Test>]
let ``multiline list of string should not add ;`` () =
    formatSourceString false """
       [ "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
         "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll" ]
"""  ({ config with MaxLineLength = 80 })
    |> should equal """[ "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
  "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll" ]
"""

[<Test>]
let ``line comment inside list parameter`` () =
    formatSourceString false """
let prismCli commando =
    let props =
        createObj [
            "component" ==> "pre"
            //"className" ==> "language-fsharp"
        ]
    ()
"""  config
    |> should equal """let prismCli commando =
    let props =
        createObj
            [ "component" ==> "pre"
              //"className" ==> "language-fsharp"
             ]

    ()
"""


[<Test>]
let ``line comment inside array parameter`` () =
    formatSourceString false """
let prismCli commando =
    let props =
        createObj [|
            "component" ==> "pre"
            //"className" ==> "language-fsharp"
        |]
    ()
"""  ({ config with SpaceAroundDelimiter = false })
    |> should equal """let prismCli commando =
    let props =
        createObj
            [|"component" ==> "pre"
              //"className" ==> "language-fsharp"
            |]

    ()
"""

[<Test>]
let ``line comment inside list`` () =
    formatSourceString false """[ 7
// foo
]
"""  ({ config with SpaceAroundDelimiter = false })
    |> should equal """[7
 // foo
]
"""

[<Test>]
let ``line comment inside array`` () =
    formatSourceString false """[| 7
// foo
|]
"""  config
    |> should equal """[| 7
   // foo
 |]
"""

[<Test>]
[<Ignore("fails on ci")>]
let ``long array sequence`` () =
    formatSourceString false """
let input =
    [|2;0;-1;2;-2;-3;1;-2;0;-8;-1;-7;-8;-11;-7;-8;-7;2;-4;-4;-6;-9;0;-20;-7;-16;-10;-25;-3;-17;0;2;0;-29;-25;-8;-2;-19;0;-38;-29;-5;-24;2;-1;-30;0;-28;-43;-22;1;-12;-23;-23;-41;-12;-51;-56;-1;-57;-29;-32;-29;-55;1;-28;-29;-49;-56;-13;-67;-40;-10;-44;-32;-37;-49;-37;-72;-54;-22;-24;-31;-26;-45;-45;-45;-23;-41;-85;-2;-90;-31;-47;-81;-40;-43;-51;-61;-41;-64;-60;-5;-29;-18;-104;-70;-68;-53;-17;-58;-107;1;-39;-36;-107;-99;-60;-13;-71;-117;-91;-117;-36;-102;-29;-23;1;-90;-69;-10;-28;-94;-92;-13;-121;-57;-16;-27;-77;-81;-45;-79;-49;-6;-14;0;-122;-87;-75;-67;-43;-113;-149;-144;-48;-6;-104;-155;-136;-85;-136;-157;-149;-25;-18;-61;-67;-34;-108;-129;-102;-9;-145;-95;-21;-144;-21;-92;-135;-121;-67;-64;0;-74;-175;-105;-120;-169;-35;-41;-92;-51;-76;-63;-184;-163;-189;-43;-58;-84;-27;-147;-147;-54;-26;-106;-83;-58;-55;-124;-145;-76;-81;-97;-88;-105;-76;0;-84;-50;-106;-105;-153;-75;-15;-40;-156;-125;-109;-17;-147;-180;-156;-94;-201;-49;-187;-13;-9;-196;-108;-120;-94;-27;-96;-68;-128;-67;-181;0;-231;-125;-33;-241;-194;-255;-41;-7;-146;-135;-70;-157;-232;-21;-153;-182;-130;-263;-222;-137;-176;-192;-84;-91;-154;-83;-69;-94;-90;-112;-44;-52;-193;-240;-5;-140;-156;-185;-176;-221;-180;-60;-71;-155;-238;-28;-165;-265;-22;-176;-89;-174;-289;-52;-110;-84;-54;-53;-250;-189;-109;-227;-309;-93;-173;-171;-168;-278;-196;-103;-130;-49;-321;-105;-1;-219;0;-81;-62;-55;-250;-257;-223;-22;-67;-55;-12;-106;-169;-243;-17;-308;-181;-310;-220;-232;-83;-12;-126;-265;-84;-273;-264;-253;-289;-119;-58;-195;-184;-340;-230;-5;-283;-68;-137;-41;-31;-210;-97;-354;-79;-267;-340;-235;-340;-82;-166;-225;-95;-68;-130;-14;-218;-239;-115;-40;-10;-20;-381;-58;-10;-328;-323;-133;-139;-224;-96;-158;-219;-112;-360;-138;-123;-303;-58;-209;-309;-91;-276;-5;-352;-66;-301;-379;-222;-88;-110;-374;-266;-334;-382;-189;-106;-260;-322;-269;-75;-117;-36;-412;-62;-285;-291;-188;-17;-158;-415;-285;-235;-223;-208;-30;-273;-276;-239;-93;0;-250;-251;-421;-368;-253;-253;-419;-364;-343;-188;-352;-147;-401;-55;-449;-171;-382;-36;-250;-306;-72;-278;-229;-69;-145;-16;-455;-474;-211;-183;-265;-306;-425;-354;-6;-256;-397;-252;-409;-126;-383;-325;-409;-431;0;-306;-52;-219;-172;-346;-444;-84;-56;-402;-112;-62;-172;-358;-329;-221;-371;-174;-388;-9;-168;-56;-109;-511;-161;-282;-344;-437;-292;-423;-308;-478;-175;-169;-468;-54;-439;-231;-357;-500;-414;-101;-53;-71;-192;-166;-517;-296;-249;-153;-9;-252;-130;-307;-240;-312;-242;-377;-48;-57;-6;-96;0;-124;-463;-68;-309;-487;-448;-172;-553;-165;-399;-223;-45;-190;-552;-209;-238;-458;-199;-154;-212;-53;-347;-316;-419;-363;-407;-435;-150;-203;-525;-159;-214;-216;-9;-302;-1;-158;-309;-33;-168;-539;-461;-171;-274;-68;-126;-372;-316;-160;-212;-261;-570;-386;-49;-494;-428;-458;-410;-419;-380;-25;-26;-36;-328;-303;-412;-169;-140;-359;-112;-198;-517;-180;-459;-550;-529;-413;-219;-223;-518;-584;-253;-552;-287;-280;-129;-187;-531;-583;-48;-222;-598;-590;-399;-488;-457;-290;-259;-624;-504;-336;-594;-435;-328;-656;-413;-195;-33;-574;-289;-60;-180;-640;-517;-26;-359;-157;-81;-503;-530;-21;-274;-84;-619;-68;-568;-483;-229;-499;-516;-406;-511;-290;-125;-90;-486;-655;-342;-347;-311;-121;-83;-53;-404;-486;-491;-155;-208;-90;-394;-379;-451;-375;-404;2;-70;-688;-516;-654;-453;-481;-111;-144;-606;-321;-113;-513;-457;-337;-638;-5;-72;-496;-136;-59;-528;-337;-445;-295;-488;-227;-226;-115;-120;-121;-103;-320;-203;-700;-257;-385;-76;-334;-254;-395;-673;-118;-202;-178;-415;-347;-491;-266;-310;-102;-266;-48;-319;-56;-433;-185;-15;-766;-694;-714;-720;-707;-491;-541;-69;-27;-21;-726;-578;-287;-544;-432;-351;-155;-614;-220;-417;-206;-496;-496;-487;-245;-634;-95;-705;-273;-342;-425;-488;-97;-710;-324;-464;-169;-298;-52;-288;-353;-71;-41;-226;-795;-46;-224;-712;-301;-559;-371;-122;-547;-446;-580;-583;-164;-812;-234;-383;-93;-112;-541;-183;-199;-171;-826;-665;-86;-300;-314;-382;-586;-471;-838;-25;-359;-352;-205;-418;-621;-387;-582;-563;-520;-649;-476;-202;-509;-121;-545;-479;-307;-614;-476;-552;-134;-198;-198;-482;-745;-680;-443;-362;-39;-353;-829;-727;-563;-66;-306;-224;-145;-182;-820;-102;-25;-307;-203;-469;-438;-74;-211;-394;-723;-406;-671;-356;-726;-792;-288;-23;-398;-459;-221;-133;-269;-661;-531;-502;-737;-666;-359;-375;-834;-629;-767;-882;-358;-865;-875;-740;-816;-661;-378;-354;-596;-729;-764;-262;-802;-374;-293;-661;-435;-168;-928;-301;-823;-470;-519;-692;-589;-939;-855;-699;-585;-632;-831;-575;-357;-871;-844;-667;-366;-772;-766;-594;-660;-302;-894;-124;-518;-216;-498;-287;-6;-334;-892;-391;-419;-236;-508;-758;-823;-824;-701;-766;-317;-186;-375;-421;-246;-362;-4;-678;-202;-506;-801;-665;-689;-547;-831;-391;-174;-475;-587;-747;-870;-164;-975;-336;-564;-248;-340;-242;-641;-510;-827;-634;-973;-196;-83;-798;-393;-414;-617;-919;-21;-129;-831;-502;-139;-858;-967;-1020;-19;-622;-878;-63;-799;-171;-277;-395;-166;-793;-745;-752;-228;-287;-997;-720;-864;-10;-578;-479;-488;-265;-1032;-909;-157;-633;-773;-1009;-61;-988;-896;-995;-792;-647;-305;-294;|]

let sample = [|0;3;0;1;-3|]
"""  config
    |> prepend newline
    |> should equal """
let input =
    [| 2
       0
       -1
       2
       -2
       -3
       1
       -2
       0
       -8
       -1
       -7
       -8
       -11
       -7
       -8
       -7
       2
       -4
       -4
       -6
       -9
       0
       -20
       -7
       -16
       -10
       -25
       -3
       -17
       0
       2
       0
       -29
       -25
       -8
       -2
       -19
       0
       -38
       -29
       -5
       -24
       2
       -1
       -30
       0
       -28
       -43
       -22
       1
       -12
       -23
       -23
       -41
       -12
       -51
       -56
       -1
       -57
       -29
       -32
       -29
       -55
       1
       -28
       -29
       -49
       -56
       -13
       -67
       -40
       -10
       -44
       -32
       -37
       -49
       -37
       -72
       -54
       -22
       -24
       -31
       -26
       -45
       -45
       -45
       -23
       -41
       -85
       -2
       -90
       -31
       -47
       -81
       -40
       -43
       -51
       -61
       -41
       -64
       -60
       -5
       -29
       -18
       -104
       -70
       -68
       -53
       -17
       -58
       -107
       1
       -39
       -36
       -107
       -99
       -60
       -13
       -71
       -117
       -91
       -117
       -36
       -102
       -29
       -23
       1
       -90
       -69
       -10
       -28
       -94
       -92
       -13
       -121
       -57
       -16
       -27
       -77
       -81
       -45
       -79
       -49
       -6
       -14
       0
       -122
       -87
       -75
       -67
       -43
       -113
       -149
       -144
       -48
       -6
       -104
       -155
       -136
       -85
       -136
       -157
       -149
       -25
       -18
       -61
       -67
       -34
       -108
       -129
       -102
       -9
       -145
       -95
       -21
       -144
       -21
       -92
       -135
       -121
       -67
       -64
       0
       -74
       -175
       -105
       -120
       -169
       -35
       -41
       -92
       -51
       -76
       -63
       -184
       -163
       -189
       -43
       -58
       -84
       -27
       -147
       -147
       -54
       -26
       -106
       -83
       -58
       -55
       -124
       -145
       -76
       -81
       -97
       -88
       -105
       -76
       0
       -84
       -50
       -106
       -105
       -153
       -75
       -15
       -40
       -156
       -125
       -109
       -17
       -147
       -180
       -156
       -94
       -201
       -49
       -187
       -13
       -9
       -196
       -108
       -120
       -94
       -27
       -96
       -68
       -128
       -67
       -181
       0
       -231
       -125
       -33
       -241
       -194
       -255
       -41
       -7
       -146
       -135
       -70
       -157
       -232
       -21
       -153
       -182
       -130
       -263
       -222
       -137
       -176
       -192
       -84
       -91
       -154
       -83
       -69
       -94
       -90
       -112
       -44
       -52
       -193
       -240
       -5
       -140
       -156
       -185
       -176
       -221
       -180
       -60
       -71
       -155
       -238
       -28
       -165
       -265
       -22
       -176
       -89
       -174
       -289
       -52
       -110
       -84
       -54
       -53
       -250
       -189
       -109
       -227
       -309
       -93
       -173
       -171
       -168
       -278
       -196
       -103
       -130
       -49
       -321
       -105
       -1
       -219
       0
       -81
       -62
       -55
       -250
       -257
       -223
       -22
       -67
       -55
       -12
       -106
       -169
       -243
       -17
       -308
       -181
       -310
       -220
       -232
       -83
       -12
       -126
       -265
       -84
       -273
       -264
       -253
       -289
       -119
       -58
       -195
       -184
       -340
       -230
       -5
       -283
       -68
       -137
       -41
       -31
       -210
       -97
       -354
       -79
       -267
       -340
       -235
       -340
       -82
       -166
       -225
       -95
       -68
       -130
       -14
       -218
       -239
       -115
       -40
       -10
       -20
       -381
       -58
       -10
       -328
       -323
       -133
       -139
       -224
       -96
       -158
       -219
       -112
       -360
       -138
       -123
       -303
       -58
       -209
       -309
       -91
       -276
       -5
       -352
       -66
       -301
       -379
       -222
       -88
       -110
       -374
       -266
       -334
       -382
       -189
       -106
       -260
       -322
       -269
       -75
       -117
       -36
       -412
       -62
       -285
       -291
       -188
       -17
       -158
       -415
       -285
       -235
       -223
       -208
       -30
       -273
       -276
       -239
       -93
       0
       -250
       -251
       -421
       -368
       -253
       -253
       -419
       -364
       -343
       -188
       -352
       -147
       -401
       -55
       -449
       -171
       -382
       -36
       -250
       -306
       -72
       -278
       -229
       -69
       -145
       -16
       -455
       -474
       -211
       -183
       -265
       -306
       -425
       -354
       -6
       -256
       -397
       -252
       -409
       -126
       -383
       -325
       -409
       -431
       0
       -306
       -52
       -219
       -172
       -346
       -444
       -84
       -56
       -402
       -112
       -62
       -172
       -358
       -329
       -221
       -371
       -174
       -388
       -9
       -168
       -56
       -109
       -511
       -161
       -282
       -344
       -437
       -292
       -423
       -308
       -478
       -175
       -169
       -468
       -54
       -439
       -231
       -357
       -500
       -414
       -101
       -53
       -71
       -192
       -166
       -517
       -296
       -249
       -153
       -9
       -252
       -130
       -307
       -240
       -312
       -242
       -377
       -48
       -57
       -6
       -96
       0
       -124
       -463
       -68
       -309
       -487
       -448
       -172
       -553
       -165
       -399
       -223
       -45
       -190
       -552
       -209
       -238
       -458
       -199
       -154
       -212
       -53
       -347
       -316
       -419
       -363
       -407
       -435
       -150
       -203
       -525
       -159
       -214
       -216
       -9
       -302
       -1
       -158
       -309
       -33
       -168
       -539
       -461
       -171
       -274
       -68
       -126
       -372
       -316
       -160
       -212
       -261
       -570
       -386
       -49
       -494
       -428
       -458
       -410
       -419
       -380
       -25
       -26
       -36
       -328
       -303
       -412
       -169
       -140
       -359
       -112
       -198
       -517
       -180
       -459
       -550
       -529
       -413
       -219
       -223
       -518
       -584
       -253
       -552
       -287
       -280
       -129
       -187
       -531
       -583
       -48
       -222
       -598
       -590
       -399
       -488
       -457
       -290
       -259
       -624
       -504
       -336
       -594
       -435
       -328
       -656
       -413
       -195
       -33
       -574
       -289
       -60
       -180
       -640
       -517
       -26
       -359
       -157
       -81
       -503
       -530
       -21
       -274
       -84
       -619
       -68
       -568
       -483
       -229
       -499
       -516
       -406
       -511
       -290
       -125
       -90
       -486
       -655
       -342
       -347
       -311
       -121
       -83
       -53
       -404
       -486
       -491
       -155
       -208
       -90
       -394
       -379
       -451
       -375
       -404
       2
       -70
       -688
       -516
       -654
       -453
       -481
       -111
       -144
       -606
       -321
       -113
       -513
       -457
       -337
       -638
       -5
       -72
       -496
       -136
       -59
       -528
       -337
       -445
       -295
       -488
       -227
       -226
       -115
       -120
       -121
       -103
       -320
       -203
       -700
       -257
       -385
       -76
       -334
       -254
       -395
       -673
       -118
       -202
       -178
       -415
       -347
       -491
       -266
       -310
       -102
       -266
       -48
       -319
       -56
       -433
       -185
       -15
       -766
       -694
       -714
       -720
       -707
       -491
       -541
       -69
       -27
       -21
       -726
       -578
       -287
       -544
       -432
       -351
       -155
       -614
       -220
       -417
       -206
       -496
       -496
       -487
       -245
       -634
       -95
       -705
       -273
       -342
       -425
       -488
       -97
       -710
       -324
       -464
       -169
       -298
       -52
       -288
       -353
       -71
       -41
       -226
       -795
       -46
       -224
       -712
       -301
       -559
       -371
       -122
       -547
       -446
       -580
       -583
       -164
       -812
       -234
       -383
       -93
       -112
       -541
       -183
       -199
       -171
       -826
       -665
       -86
       -300
       -314
       -382
       -586
       -471
       -838
       -25
       -359
       -352
       -205
       -418
       -621
       -387
       -582
       -563
       -520
       -649
       -476
       -202
       -509
       -121
       -545
       -479
       -307
       -614
       -476
       -552
       -134
       -198
       -198
       -482
       -745
       -680
       -443
       -362
       -39
       -353
       -829
       -727
       -563
       -66
       -306
       -224
       -145
       -182
       -820
       -102
       -25
       -307
       -203
       -469
       -438
       -74
       -211
       -394
       -723
       -406
       -671
       -356
       -726
       -792
       -288
       -23
       -398
       -459
       -221
       -133
       -269
       -661
       -531
       -502
       -737
       -666
       -359
       -375
       -834
       -629
       -767
       -882
       -358
       -865
       -875
       -740
       -816
       -661
       -378
       -354
       -596
       -729
       -764
       -262
       -802
       -374
       -293
       -661
       -435
       -168
       -928
       -301
       -823
       -470
       -519
       -692
       -589
       -939
       -855
       -699
       -585
       -632
       -831
       -575
       -357
       -871
       -844
       -667
       -366
       -772
       -766
       -594
       -660
       -302
       -894
       -124
       -518
       -216
       -498
       -287
       -6
       -334
       -892
       -391
       -419
       -236
       -508
       -758
       -823
       -824
       -701
       -766
       -317
       -186
       -375
       -421
       -246
       -362
       -4
       -678
       -202
       -506
       -801
       -665
       -689
       -547
       -831
       -391
       -174
       -475
       -587
       -747
       -870
       -164
       -975
       -336
       -564
       -248
       -340
       -242
       -641
       -510
       -827
       -634
       -973
       -196
       -83
       -798
       -393
       -414
       -617
       -919
       -21
       -129
       -831
       -502
       -139
       -858
       -967
       -1020
       -19
       -622
       -878
       -63
       -799
       -171
       -277
       -395
       -166
       -793
       -745
       -752
       -228
       -287
       -997
       -720
       -864
       -10
       -578
       -479
       -488
       -265
       -1032
       -909
       -157
       -633
       -773
       -1009
       -61
       -988
       -896
       -995
       -792
       -647
       -305
       -294 |]

let sample = [| 0; 3; 0; 1; -3 |]
"""

[<Test>]
let ``F# 4.7 implicit yield in sequence`` () =
    formatSourceString false """seq { 1;2;3; }
"""  config
    |> prepend newline
    |> should equal """
seq {
    1
    2
    3
}
"""

[<Test>]
let ``F# 4.7 implicit yield in list`` () =
    formatSourceString false """
let f' includeWeekend =
    [
        "Monday"
        "Tuesday"
        "Wednesday"
        "Thursday"
        "Friday"
        if includeWeekend then
            "Saturday"
            "Sunday"
    ]
"""  config
    |> prepend newline
    |> should equal """
let f' includeWeekend =
    [ "Monday"
      "Tuesday"
      "Wednesday"
      "Thursday"
      "Friday"
      if includeWeekend then
          "Saturday"
          "Sunday" ]
"""

[<Test>]
let NamedIndexedPropertySet () =
    formatSourceString false """analysisKey.Headers.Item(key) <- value
"""  config
    |> prepend newline
    |> should equal """
analysisKey.Headers.Item(key) <- value
"""

[<Test>]
let DotNamedIndexedPropertySet () =
    formatSourceString false """(foo()).Item(key) <- value
"""  config
    |> prepend newline
    |> should equal """
(foo ()).Item(key) <- value
"""

[<Test>]
let ``comment after [ should be preserved, 551`` () =
    formatSourceString false """
let nestedList: obj list = [
    "11111111aaaaaaaaa"
    "22222222aaaaaaaaa"
    "33333333aaaaaaaaa"
    [ // this case looks weird but seen rarely
        "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb"
    ]
]
"""  ({ config with MaxLineLength = 80 })
    |> prepend newline
    |> should equal """
let nestedList: obj list =
    [ "11111111aaaaaaaaa"
      "22222222aaaaaaaaa"
      "33333333aaaaaaaaa"
      [ // this case looks weird but seen rarely
        "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb" ] ]
"""


[<Test>]
let ``comment after [| should be preserved, 551`` () =
    formatSourceString false """
let nestedList: obj list = [|
    "11111111aaaaaaaaa"
    "22222222aaaaaaaaa"
    "33333333aaaaaaaaa"
    [| // this case looks weird but seen rarely
        "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb"
    |]
|]
"""  ({ config with MaxLineLength = 80 })
    |> prepend newline
    |> should equal """
let nestedList: obj list =
    [| "11111111aaaaaaaaa"
       "22222222aaaaaaaaa"
       "33333333aaaaaaaaa"
       [| // this case looks weird but seen rarely
          "11111111bbbbbbbbbbbbbbb"
          "22222222bbbbbbbbbbbbbbb"
          "33333333bbbbbbbbbbbbbbb" |] |]
"""

[<Test>]
let ``comment after |] should be preserved, 551`` () =
    formatSourceString false """
let nestedList: obj list = [|
    "11111111aaaaaaaaa"
    "22222222aaaaaaaaa"
    "33333333aaaaaaaaa"
    [|  // this case looks weird but seen rarely
        "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb"
    |]
|]
"""  ({ config with MaxLineLength = 80 })
    |> prepend newline
    |> should equal """
let nestedList: obj list =
    [| "11111111aaaaaaaaa"
       "22222222aaaaaaaaa"
       "33333333aaaaaaaaa"
       [| // this case looks weird but seen rarely
          "11111111bbbbbbbbbbbbbbb"
          "22222222bbbbbbbbbbbbbbb"
          "33333333bbbbbbbbbbbbbbb" |] |]
"""

[<Test>]
let ``line comment before nested ] should be preserved`` () =
    formatSourceString false """
let nestedList: obj list = [
    "11111111aaaaaaaaa"
    "22222222aaaaaaaaa"
    "33333333aaaaaaaaa"
    [
        "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb"
        // this case looks weird but seen rarely
    ]
]
"""  ({ config with MaxLineLength = 80 })
    |> prepend newline
    |> should equal """
let nestedList: obj list =
    [ "11111111aaaaaaaaa"
      "22222222aaaaaaaaa"
      "33333333aaaaaaaaa"
      [ "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb"
        // this case looks weird but seen rarely
       ] ]
"""

[<Test>]
let ``line comment before nested |] should be preserved`` () =
    formatSourceString false """
let nestedList: obj list = [|
    "11111111aaaaaaaaa"
    "22222222aaaaaaaaa"
    "33333333aaaaaaaaa"
    [|
        "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb"
        // this case looks weird but seen rarely
    |]
|]
"""  ({ config with MaxLineLength = 80 })
    |> prepend newline
    |> should equal """
let nestedList: obj list =
    [| "11111111aaaaaaaaa"
       "22222222aaaaaaaaa"
       "33333333aaaaaaaaa"
       [| "11111111bbbbbbbbbbbbbbb"
          "22222222bbbbbbbbbbbbbbb"
          "33333333bbbbbbbbbbbbbbb"
          // this case looks weird but seen rarely
        |] |]
"""

[<Test>]
let ``from-end slicing with lists`` () =
    formatSourceString false """
let a = list.[..^0]   // 1,2,3,4,5
let b = list.[..^1]   // 1,2,3,4
let c = list.[0..^1]  // 1,2,3,4
let d = list.[^1..]   // 4,5
let e = list.[^0..]   // 5
let f = list.[^2..^1] // 3,4
"""  ({ config with MaxLineLength = 80 })
    |> prepend newline
    |> should equal """
let a = list.[..^0] // 1,2,3,4,5
let b = list.[..^1] // 1,2,3,4
let c = list.[0..^1] // 1,2,3,4
let d = list.[^1..] // 4,5
let e = list.[^0..] // 5
let f = list.[^2..^1] // 3,4
"""

[<Test>]
let ``calling indexed item in list, 798`` () =
    formatSourceString false """namespace Foo

type T = { A : (unit -> unit) array }
module F =
  let f (a : T) =
    a.A.[0] ()
"""  config
    |> prepend newline
    |> should equal """
namespace Foo

type T = { A: (unit -> unit) array }

module F =
    let f (a: T) = a.A.[0]()
"""
