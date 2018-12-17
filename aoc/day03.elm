import Browser
import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import Dict exposing (Dict)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, spaces)

type alias Claim =
  { id : Int
  , x : Int
  , y : Int
  , w : Int
  , h : Int
  }


myInput = "#1 @ 935,649: 22x22"

-- myInput = """
-- #1 @ 935,649: 22x22
-- #2 @ 346,47: 19x26
-- #3 @ 218,455: 25x17
-- #4 @ 451,711: 10x20
-- #5 @ 797,342: 28x27
-- #6 @ 97,281: 23x13
-- #7 @ 752,418: 25x26
-- #8 @ 181,975: 11x20
-- #9 @ 986,373: 13x18
-- #10 @ 734,260: 21x18
-- #11 @ 174,822: 15x14
-- #12 @ 339,138: 20x28
-- #13 @ 190,315: 20x14
-- #14 @ 890,631: 26x29
-- #15 @ 967,390: 16x10
-- #16 @ 314,205: 10x23
-- #17 @ 199,93: 14x20
-- #18 @ 965,143: 19x29
-- #19 @ 876,70: 15x18
-- #20 @ 671,254: 11x25
-- #21 @ 755,229: 14x18
-- #22 @ 578,727: 24x14
-- #23 @ 298,459: 11x25
-- #24 @ 593,793: 25x28
-- #25 @ 377,777: 15x26
-- #26 @ 710,224: 3x18
-- #27 @ 836,814: 18x16
-- #28 @ 10,413: 21x15
-- #29 @ 961,500: 27x21
-- #30 @ 714,515: 25x13
-- #31 @ 47,240: 21x25
-- #32 @ 503,203: 27x29
-- #33 @ 488,463: 25x15
-- #34 @ 139,935: 11x16
-- #35 @ 936,573: 16x25
-- #36 @ 510,958: 28x18
-- #37 @ 818,447: 27x21
-- #38 @ 440,606: 27x25
-- #39 @ 738,548: 21x21
-- #40 @ 251,176: 14x25
-- #41 @ 916,835: 25x13
-- #42 @ 241,462: 21x18
-- #43 @ 694,184: 28x22
-- #44 @ 26,363: 13x28
-- #45 @ 754,356: 13x12
-- #46 @ 647,256: 14x14
-- #47 @ 273,259: 17x25
-- #48 @ 274,299: 15x17
-- #49 @ 775,108: 25x22
-- #50 @ 313,73: 17x21
-- #51 @ 914,790: 19x18
-- #52 @ 699,965: 23x13
-- #53 @ 480,607: 16x27
-- #54 @ 493,134: 10x26
-- #55 @ 868,661: 16x19
-- #56 @ 402,792: 11x14
-- #57 @ 129,606: 12x12
-- #58 @ 190,654: 11x17
-- #59 @ 32,628: 23x17
-- #60 @ 875,227: 20x21
-- #61 @ 28,937: 29x14
-- #62 @ 416,864: 27x18
-- #63 @ 94,378: 15x13
-- #64 @ 40,774: 18x11
-- #65 @ 283,547: 21x16
-- #66 @ 245,31: 29x12
-- #67 @ 268,618: 12x26
-- #68 @ 973,980: 20x16
-- #69 @ 492,240: 10x18
-- #70 @ 898,385: 12x22
-- #71 @ 465,782: 14x24
-- #72 @ 350,985: 14x15
-- #73 @ 804,941: 15x24
-- #74 @ 618,624: 14x13
-- #75 @ 269,540: 27x19
-- #76 @ 220,949: 10x18
-- #77 @ 577,299: 19x18
-- #78 @ 311,132: 26x17
-- #79 @ 202,330: 26x11
-- #80 @ 564,413: 19x21
-- #81 @ 38,472: 19x28
-- #82 @ 352,670: 15x11
-- #83 @ 39,532: 11x28
-- #84 @ 375,832: 24x16
-- #85 @ 453,232: 25x17
-- #86 @ 649,571: 14x12
-- #87 @ 876,324: 28x28
-- #88 @ 932,177: 11x20
-- #89 @ 709,241: 24x10
-- #90 @ 698,546: 12x21
-- #91 @ 773,51: 15x17
-- #92 @ 113,436: 16x12
-- #93 @ 598,794: 11x27
-- #94 @ 581,437: 14x14
-- #95 @ 787,620: 29x15
-- #96 @ 8,593: 10x21
-- #97 @ 845,607: 19x22
-- #98 @ 189,672: 16x15
-- #99 @ 335,901: 28x17
-- #100 @ 52,312: 14x16
-- #101 @ 13,487: 21x13
-- #102 @ 937,624: 15x23
-- #103 @ 148,804: 21x18
-- #104 @ 707,146: 21x18
-- #105 @ 188,507: 27x28
-- #106 @ 450,647: 12x14
-- #107 @ 430,250: 11x27
-- #108 @ 279,519: 26x28
-- #109 @ 736,495: 24x12
-- #110 @ 693,454: 13x12
-- #111 @ 982,624: 12x12
-- #112 @ 409,713: 22x26
-- #113 @ 644,311: 13x14
-- #114 @ 106,418: 20x16
-- #115 @ 714,740: 19x18
-- #116 @ 732,814: 13x15
-- #117 @ 756,213: 10x27
-- #118 @ 336,230: 27x18
-- #119 @ 817,358: 17x20
-- #120 @ 913,779: 29x28
-- #121 @ 351,913: 29x23
-- #122 @ 707,408: 19x10
-- #123 @ 195,666: 12x19
-- #124 @ 688,451: 14x25
-- #125 @ 851,822: 23x26
-- #126 @ 832,839: 25x12
-- #127 @ 434,38: 12x21
-- #128 @ 627,42: 29x29
-- #129 @ 340,78: 20x10
-- #130 @ 402,328: 26x24
-- #131 @ 257,741: 21x16
-- #132 @ 677,67: 12x13
-- #133 @ 140,703: 12x21
-- #134 @ 522,165: 11x24
-- #135 @ 407,840: 18x16
-- #136 @ 625,712: 21x14
-- #137 @ 850,727: 26x11
-- #138 @ 428,851: 24x20
-- #139 @ 351,537: 11x26
-- #140 @ 830,896: 16x14
-- #141 @ 837,324: 29x22
-- #142 @ 485,26: 21x17
-- #143 @ 121,861: 24x15
-- #144 @ 220,960: 29x12
-- #145 @ 74,235: 14x23
-- #146 @ 354,728: 22x28
-- #147 @ 351,776: 21x18
-- #148 @ 508,64: 18x26
-- #149 @ 795,45: 10x19
-- #150 @ 436,400: 28x22
-- #151 @ 958,248: 24x26
-- #152 @ 388,818: 14x24
-- #153 @ 957,901: 19x21
-- #154 @ 952,755: 12x21
-- #155 @ 763,242: 14x16
-- #156 @ 63,483: 24x11
-- #157 @ 319,572: 13x16
-- #158 @ 477,701: 10x25
-- #159 @ 557,537: 20x15
-- #160 @ 331,915: 24x16
-- #161 @ 877,956: 17x14
-- #162 @ 956,570: 29x16
-- #163 @ 490,200: 18x29
-- #164 @ 596,525: 22x22
-- #165 @ 311,935: 27x11
-- #166 @ 218,318: 22x12
-- #167 @ 533,766: 27x25
-- #168 @ 788,881: 10x10
-- #169 @ 36,179: 26x29
-- #170 @ 122,316: 28x20
-- #171 @ 153,55: 29x21
-- #172 @ 388,618: 29x24
-- #173 @ 749,344: 25x15
-- #174 @ 466,726: 23x16
-- #175 @ 437,461: 22x26
-- #176 @ 762,263: 15x18
-- #177 @ 810,883: 27x14
-- #178 @ 400,490: 12x25
-- #179 @ 429,295: 23x10
-- #180 @ 542,364: 18x17
-- #181 @ 807,445: 24x26
-- #182 @ 271,252: 13x21
-- #183 @ 439,682: 29x21
-- #184 @ 335,725: 20x27
-- #185 @ 158,58: 11x11
-- #186 @ 167,546: 22x11
-- #187 @ 13,803: 19x28
-- #188 @ 775,527: 23x24
-- #189 @ 920,169: 28x19
-- #190 @ 908,823: 28x20
-- #191 @ 391,264: 18x20
-- #192 @ 550,919: 20x17
-- #193 @ 788,392: 26x13
-- #194 @ 160,393: 13x20
-- #195 @ 226,638: 12x18
-- #196 @ 809,602: 16x18
-- #197 @ 33,372: 18x21
-- #198 @ 591,865: 23x15
-- #199 @ 796,765: 25x19
-- #200 @ 429,599: 24x13
-- #201 @ 584,391: 21x17
-- #202 @ 336,192: 18x13
-- #203 @ 883,30: 24x28
-- #204 @ 790,195: 27x12
-- #205 @ 342,304: 16x19
-- #206 @ 348,758: 22x27
-- #207 @ 336,133: 11x13
-- #208 @ 313,96: 11x17
-- #209 @ 315,922: 27x20
-- #210 @ 802,266: 14x23
-- #211 @ 415,178: 25x20
-- #212 @ 43,945: 16x29
-- #213 @ 647,70: 29x12
-- #214 @ 800,590: 18x14
-- #215 @ 362,907: 17x17
-- #216 @ 586,773: 24x17
-- #217 @ 125,913: 26x24
-- #218 @ 363,757: 22x19
-- #219 @ 530,368: 16x13
-- #220 @ 366,455: 16x17
-- #221 @ 331,134: 10x18
-- #222 @ 552,307: 18x27
-- #223 @ 972,366: 15x27
-- #224 @ 252,150: 20x11
-- #225 @ 41,111: 12x13
-- #226 @ 364,457: 29x11
-- #227 @ 16,166: 16x10
-- #228 @ 965,784: 19x18
-- #229 @ 682,625: 17x13
-- #230 @ 872,213: 28x28
-- #231 @ 38,985: 26x10
-- #232 @ 276,335: 12x24
-- #233 @ 863,940: 28x20
-- #234 @ 321,797: 19x16
-- #235 @ 124,222: 22x10
-- #236 @ 731,31: 28x19
-- #237 @ 185,799: 19x14
-- #238 @ 166,434: 24x10
-- #239 @ 913,805: 29x14
-- #240 @ 355,617: 18x25
-- #241 @ 361,976: 29x23
-- #242 @ 188,219: 15x17
-- #243 @ 63,460: 24x28
-- #244 @ 288,953: 20x15
-- #245 @ 913,922: 14x14
-- #246 @ 22,167: 21x15
-- #247 @ 755,554: 28x18
-- #248 @ 560,542: 9x6
-- #249 @ 59,287: 22x16
-- #250 @ 953,249: 11x16
-- #251 @ 564,433: 21x11
-- #252 @ 506,356: 15x23
-- #253 @ 328,197: 22x24
-- #254 @ 404,378: 28x15
-- #255 @ 39,385: 10x16
-- #256 @ 619,580: 14x26
-- #257 @ 818,825: 28x15
-- #258 @ 893,810: 16x21
-- #259 @ 127,161: 21x23
-- #260 @ 959,973: 21x19
-- #261 @ 299,124: 24x19
-- #262 @ 91,602: 18x25
-- #263 @ 502,614: 3x22
-- #264 @ 770,852: 20x19
-- #265 @ 191,544: 24x28
-- #266 @ 78,714: 12x27
-- #267 @ 865,161: 15x14
-- #268 @ 577,861: 29x12
-- #269 @ 265,710: 19x10
-- #270 @ 252,982: 23x12
-- #271 @ 792,29: 11x29
-- #272 @ 845,696: 13x13
-- #273 @ 712,974: 26x25
-- #274 @ 893,231: 23x29
-- #275 @ 170,689: 21x11
-- #276 @ 520,905: 16x22
-- #277 @ 103,603: 11x19
-- #278 @ 45,954: 28x23
-- #279 @ 111,699: 22x29
-- #280 @ 122,43: 13x14
-- #281 @ 438,579: 13x13
-- #282 @ 725,767: 21x13
-- #283 @ 63,811: 22x22
-- #284 @ 492,286: 10x19
-- #285 @ 432,541: 16x26
-- #286 @ 171,39: 21x21
-- #287 @ 620,623: 24x27
-- #288 @ 33,810: 12x28
-- #289 @ 577,736: 17x16
-- #290 @ 332,276: 24x18
-- #291 @ 951,387: 26x21
-- #292 @ 302,683: 13x28
-- #293 @ 392,795: 14x16
-- #294 @ 965,780: 25x17
-- #295 @ 47,566: 17x10
-- #296 @ 311,859: 24x18
-- #297 @ 170,776: 16x29
-- #298 @ 780,941: 25x18
-- #299 @ 121,59: 22x11
-- #300 @ 125,748: 23x23
-- #301 @ 573,48: 12x28
-- #302 @ 88,300: 22x14
-- #303 @ 77,602: 26x20
-- #304 @ 264,553: 24x24
-- #305 @ 66,424: 24x27
-- #306 @ 802,659: 15x22
-- #307 @ 187,924: 20x13
-- #308 @ 381,252: 21x25
-- #309 @ 780,258: 19x26
-- #310 @ 75,914: 29x11
-- #311 @ 381,978: 13x12
-- #312 @ 721,175: 21x15
-- #313 @ 821,445: 29x20
-- #314 @ 702,41: 20x23
-- #315 @ 888,370: 22x20
-- #316 @ 528,393: 27x21
-- #317 @ 984,555: 4x4
-- #318 @ 983,632: 16x13
-- #319 @ 644,36: 23x21
-- #320 @ 525,925: 14x14
-- #321 @ 165,701: 23x27
-- #322 @ 427,858: 24x16
-- #323 @ 549,276: 11x23
-- #324 @ 821,274: 16x17
-- #325 @ 28,625: 17x22
-- #326 @ 581,878: 25x10
-- #327 @ 897,632: 19x28
-- #328 @ 308,114: 24x24
-- #329 @ 201,762: 18x20
-- #330 @ 883,693: 23x25
-- #331 @ 657,878: 19x19
-- #332 @ 426,866: 28x15
-- #333 @ 319,305: 27x14
-- #334 @ 327,918: 17x10
-- #335 @ 327,317: 25x19
-- #336 @ 960,362: 19x18
-- #337 @ 572,404: 14x21
-- #338 @ 45,287: 22x18
-- #339 @ 552,489: 26x28
-- #340 @ 328,670: 10x20
-- #341 @ 559,634: 29x28
-- #342 @ 174,409: 19x28
-- #343 @ 873,136: 23x10
-- #344 @ 261,70: 18x14
-- #345 @ 702,524: 23x18
-- #346 @ 295,647: 25x20
-- #347 @ 65,433: 19x27
-- #348 @ 704,723: 28x21
-- #349 @ 115,916: 20x19
-- #350 @ 418,640: 22x24
-- #351 @ 263,351: 28x24
-- #352 @ 931,293: 18x18
-- #353 @ 19,873: 19x28
-- #354 @ 436,194: 18x12
-- #355 @ 112,783: 23x24
-- #356 @ 412,488: 16x12
-- #357 @ 260,453: 29x13
-- #358 @ 412,259: 21x18
-- #359 @ 482,912: 15x20
-- #360 @ 957,173: 10x22
-- #361 @ 109,441: 24x13
-- #362 @ 805,826: 10x24
-- #363 @ 680,619: 10x21
-- #364 @ 154,804: 29x27
-- #365 @ 670,299: 23x24
-- #366 @ 33,964: 17x24
-- #367 @ 771,320: 28x27
-- #368 @ 780,195: 18x11
-- #369 @ 828,622: 24x16
-- #370 @ 311,786: 15x15
-- #371 @ 107,671: 28x14
-- #372 @ 958,438: 29x16
-- #373 @ 932,472: 24x20
-- #374 @ 569,534: 28x11
-- #375 @ 692,571: 9x14
-- #376 @ 322,646: 12x29
-- #377 @ 285,458: 26x12
-- #378 @ 636,707: 28x14
-- #379 @ 484,602: 13x28
-- #380 @ 458,655: 14x24
-- #381 @ 547,203: 25x25
-- #382 @ 359,567: 18x26
-- #383 @ 880,312: 25x22
-- #384 @ 489,330: 19x20
-- #385 @ 760,708: 16x20
-- #386 @ 506,58: 13x26
-- #387 @ 705,231: 12x22
-- #388 @ 274,351: 13x17
-- #389 @ 868,519: 13x29
-- #390 @ 806,655: 29x28
-- #391 @ 858,726: 16x10
-- #392 @ 427,438: 26x27
-- #393 @ 542,806: 11x13
-- #394 @ 119,663: 20x24
-- #395 @ 158,690: 17x27
-- #396 @ 961,199: 10x23
-- #397 @ 524,303: 19x14
-- #398 @ 913,246: 11x29
-- #399 @ 696,311: 27x14
-- #400 @ 921,639: 20x27
-- #401 @ 526,70: 13x18
-- #402 @ 824,446: 12x25
-- #403 @ 201,97: 6x9
-- #404 @ 516,176: 22x22
-- #405 @ 652,267: 17x20
-- #406 @ 396,249: 16x29
-- #407 @ 962,641: 11x24
-- #408 @ 430,15: 26x21
-- #409 @ 480,35: 23x24
-- #410 @ 430,610: 21x15
-- #411 @ 975,935: 11x16
-- #412 @ 705,395: 14x26
-- #413 @ 338,792: 15x25
-- #414 @ 801,345: 6x20
-- #415 @ 29,601: 13x12
-- #416 @ 772,388: 23x14
-- #417 @ 507,307: 15x21
-- #418 @ 55,683: 20x15
-- #419 @ 837,593: 27x12
-- #420 @ 10,402: 17x14
-- #421 @ 273,260: 10x17
-- #422 @ 21,938: 10x18
-- #423 @ 951,768: 10x20
-- #424 @ 681,830: 13x26
-- #425 @ 558,417: 24x17
-- #426 @ 766,391: 23x24
-- #427 @ 693,410: 18x13
-- #428 @ 67,733: 14x16
-- #429 @ 950,373: 23x11
-- #430 @ 470,504: 15x15
-- #431 @ 851,800: 15x16
-- #432 @ 596,267: 11x25
-- #433 @ 794,511: 15x17
-- #434 @ 134,532: 11x22
-- #435 @ 360,489: 13x19
-- #436 @ 757,314: 22x13
-- #437 @ 456,493: 12x20
-- #438 @ 414,721: 10x11
-- #439 @ 392,851: 23x16
-- #440 @ 744,161: 19x18
-- #441 @ 411,130: 22x17
-- #442 @ 385,481: 16x24
-- #443 @ 187,644: 5x8
-- #444 @ 459,548: 19x12
-- #445 @ 919,442: 17x18
-- #446 @ 967,734: 27x23
-- #447 @ 159,80: 25x20
-- #448 @ 20,160: 19x16
-- #449 @ 4,829: 20x11
-- #450 @ 116,627: 14x12
-- #451 @ 262,813: 14x29
-- #452 @ 259,66: 28x22
-- #453 @ 730,170: 19x23
-- #454 @ 53,257: 15x21
-- #455 @ 688,326: 17x20
-- #456 @ 865,201: 16x20
-- #457 @ 311,137: 13x29
-- #458 @ 516,776: 22x23
-- #459 @ 681,309: 23x18
-- #460 @ 694,252: 18x15
-- #461 @ 647,701: 18x14
-- #462 @ 29,977: 13x11
-- #463 @ 881,876: 25x29
-- #464 @ 942,533: 23x24
-- #465 @ 679,147: 17x22
-- #466 @ 331,706: 21x11
-- #467 @ 89,806: 17x27
-- #468 @ 878,650: 21x15
-- #469 @ 980,22: 10x17
-- #470 @ 955,872: 16x19
-- #471 @ 177,973: 21x26
-- #472 @ 0,497: 18x10
-- #473 @ 598,15: 14x22
-- #474 @ 703,256: 26x14
-- #475 @ 609,906: 16x27
-- #476 @ 212,756: 17x27
-- #477 @ 143,322: 16x17
-- #478 @ 114,108: 11x11
-- #479 @ 214,140: 24x18
-- #480 @ 275,3: 21x25
-- #481 @ 744,128: 10x10
-- #482 @ 537,146: 17x14
-- #483 @ 270,59: 20x25
-- #484 @ 63,229: 13x18
-- #485 @ 819,402: 22x27
-- #486 @ 874,231: 21x14
-- #487 @ 915,580: 28x27
-- #488 @ 2,812: 15x23
-- #489 @ 338,827: 23x16
-- #490 @ 370,987: 28x11
-- #491 @ 704,147: 28x12
-- #492 @ 484,246: 16x19
-- #493 @ 428,67: 14x14
-- #494 @ 324,901: 12x26
-- #495 @ 221,656: 17x27
-- #496 @ 354,758: 24x23
-- #497 @ 80,613: 25x23
-- #498 @ 810,907: 26x12
-- #499 @ 821,846: 14x20
-- #500 @ 694,344: 10x22
-- #501 @ 605,588: 12x26
-- #502 @ 804,117: 15x11
-- #503 @ 973,256: 15x25
-- #504 @ 862,370: 20x23
-- #505 @ 337,823: 19x26
-- #506 @ 560,584: 29x21
-- #507 @ 108,276: 20x18
-- #508 @ 46,200: 21x16
-- #509 @ 452,412: 29x24
-- #510 @ 438,254: 18x21
-- #511 @ 686,292: 19x24
-- #512 @ 122,382: 29x22
-- #513 @ 95,936: 16x23
-- #514 @ 783,471: 11x25
-- #515 @ 74,183: 22x28
-- #516 @ 24,949: 13x10
-- #517 @ 49,819: 22x11
-- #518 @ 863,973: 22x22
-- #519 @ 792,700: 10x26
-- #520 @ 243,974: 21x21
-- #521 @ 283,63: 19x15
-- #522 @ 284,0: 13x22
-- #523 @ 975,5: 13x26
-- #524 @ 347,326: 17x15
-- #525 @ 754,292: 25x29
-- #526 @ 352,279: 23x17
-- #527 @ 125,705: 17x20
-- #528 @ 497,375: 22x17
-- #529 @ 302,497: 16x28
-- #530 @ 251,302: 11x11
-- #531 @ 400,181: 27x23
-- #532 @ 373,463: 14x17
-- #533 @ 12,707: 22x10
-- #534 @ 605,764: 14x21
-- #535 @ 99,280: 16x17
-- #536 @ 620,616: 23x12
-- #537 @ 732,485: 17x10
-- #538 @ 166,905: 29x19
-- #539 @ 61,484: 24x22
-- #540 @ 656,444: 20x17
-- #541 @ 531,954: 11x21
-- #542 @ 214,181: 15x20
-- #543 @ 877,200: 26x12
-- #544 @ 539,756: 17x16
-- #545 @ 819,677: 18x25
-- #546 @ 909,638: 18x11
-- #547 @ 957,692: 25x27
-- #548 @ 780,614: 22x15
-- #549 @ 320,125: 7x4
-- #550 @ 748,277: 10x19
-- #551 @ 332,19: 23x22
-- #552 @ 218,135: 26x27
-- #553 @ 404,762: 27x22
-- #554 @ 36,451: 24x12
-- #555 @ 410,641: 13x26
-- #556 @ 457,415: 12x10
-- #557 @ 668,549: 15x16
-- #558 @ 450,246: 15x24
-- #559 @ 945,823: 24x10
-- #560 @ 675,40: 21x25
-- #561 @ 313,926: 26x11
-- #562 @ 393,953: 15x27
-- #563 @ 394,280: 29x15
-- #564 @ 224,304: 20x23
-- #565 @ 571,481: 23x22
-- #566 @ 419,301: 18x29
-- #567 @ 316,244: 11x22
-- #568 @ 6,388: 14x27
-- #569 @ 349,201: 12x10
-- #570 @ 681,42: 18x12
-- #571 @ 721,54: 15x27
-- #572 @ 40,193: 6x9
-- #573 @ 3,626: 10x20
-- #574 @ 57,575: 24x19
-- #575 @ 423,75: 20x21
-- #576 @ 539,311: 27x20
-- #577 @ 853,638: 16x22
-- #578 @ 227,544: 29x20
-- #579 @ 401,74: 11x22
-- #580 @ 672,452: 12x20
-- #581 @ 873,826: 26x11
-- #582 @ 175,227: 17x17
-- #583 @ 291,10: 27x28
-- #584 @ 141,316: 20x27
-- #585 @ 269,19: 13x18
-- #586 @ 53,622: 17x24
-- #587 @ 569,50: 18x20
-- #588 @ 786,698: 22x13
-- #589 @ 973,584: 21x16
-- #590 @ 743,251: 24x11
-- #591 @ 300,558: 21x21
-- #592 @ 679,682: 20x10
-- #593 @ 883,201: 23x22
-- #594 @ 734,381: 25x18
-- #595 @ 51,864: 24x12
-- #596 @ 831,380: 23x11
-- #597 @ 113,602: 15x20
-- #598 @ 718,435: 21x24
-- #599 @ 839,78: 21x19
-- #600 @ 820,838: 29x15
-- #601 @ 93,304: 22x21
-- #602 @ 959,640: 12x23
-- #603 @ 653,442: 24x18
-- #604 @ 401,184: 10x28
-- #605 @ 790,878: 17x11
-- #606 @ 616,181: 13x13
-- #607 @ 260,258: 28x16
-- #608 @ 126,792: 25x28
-- #609 @ 120,802: 13x27
-- #610 @ 804,187: 13x14
-- #611 @ 559,739: 23x28
-- #612 @ 815,757: 18x11
-- #613 @ 502,569: 17x16
-- #614 @ 807,390: 29x26
-- #615 @ 626,191: 10x25
-- #616 @ 66,493: 17x18
-- #617 @ 925,488: 16x10
-- #618 @ 910,645: 17x17
-- #619 @ 185,423: 22x16
-- #620 @ 796,109: 15x14
-- #621 @ 801,975: 29x23
-- #622 @ 266,25: 21x29
-- #623 @ 821,185: 25x25
-- #624 @ 257,832: 15x19
-- #625 @ 219,108: 18x21
-- #626 @ 119,40: 21x29
-- #627 @ 451,891: 28x27
-- #628 @ 810,432: 10x16
-- #629 @ 5,382: 26x11
-- #630 @ 494,538: 21x22
-- #631 @ 620,203: 14x23
-- #632 @ 250,289: 29x26
-- #633 @ 761,967: 14x19
-- #634 @ 928,661: 18x12
-- #635 @ 547,170: 29x17
-- #636 @ 244,597: 18x12
-- #637 @ 415,394: 20x14
-- #638 @ 320,108: 28x21
-- #639 @ 337,696: 19x16
-- #640 @ 36,831: 20x27
-- #641 @ 891,514: 13x10
-- #642 @ 862,592: 14x25
-- #643 @ 101,176: 11x27
-- #644 @ 287,693: 21x27
-- #645 @ 251,368: 19x25
-- #646 @ 268,307: 10x24
-- #647 @ 902,1: 20x11
-- #648 @ 166,902: 16x23
-- #649 @ 830,451: 10x24
-- #650 @ 227,220: 14x26
-- #651 @ 205,108: 29x29
-- #652 @ 406,375: 28x22
-- #653 @ 824,270: 24x22
-- #654 @ 197,332: 10x11
-- #655 @ 576,730: 15x15
-- #656 @ 617,770: 16x26
-- #657 @ 246,728: 24x19
-- #658 @ 965,848: 15x25
-- #659 @ 809,933: 11x20
-- #660 @ 198,897: 27x17
-- #661 @ 186,489: 20x29
-- #662 @ 239,651: 10x18
-- #663 @ 884,295: 22x22
-- #664 @ 147,226: 11x20
-- #665 @ 10,946: 6x14
-- #666 @ 493,544: 25x12
-- #667 @ 164,176: 12x11
-- #668 @ 141,531: 21x18
-- #669 @ 689,416: 15x12
-- #670 @ 605,78: 23x22
-- #671 @ 544,408: 24x23
-- #672 @ 560,387: 27x28
-- #673 @ 903,760: 16x19
-- #674 @ 108,420: 28x13
-- #675 @ 398,787: 11x10
-- #676 @ 872,702: 19x27
-- #677 @ 661,260: 29x20
-- #678 @ 529,581: 25x15
-- #679 @ 252,599: 5x6
-- #680 @ 173,363: 28x22
-- #681 @ 469,446: 25x29
-- #682 @ 883,613: 17x10
-- #683 @ 97,608: 4x12
-- #684 @ 917,587: 23x13
-- #685 @ 888,522: 17x23
-- #686 @ 663,241: 14x18
-- #687 @ 93,272: 18x11
-- #688 @ 174,357: 15x19
-- #689 @ 247,984: 15x14
-- #690 @ 817,846: 28x13
-- #691 @ 587,292: 12x10
-- #692 @ 350,130: 18x25
-- #693 @ 330,175: 17x23
-- #694 @ 126,828: 14x23
-- #695 @ 606,481: 28x11
-- #696 @ 887,781: 15x29
-- #697 @ 121,569: 29x17
-- #698 @ 167,524: 16x25
-- #699 @ 663,71: 25x16
-- #700 @ 372,281: 28x29
-- #701 @ 103,191: 26x14
-- #702 @ 864,373: 18x29
-- #703 @ 91,267: 17x17
-- #704 @ 195,408: 16x19
-- #705 @ 107,953: 27x22
-- #706 @ 173,917: 22x15
-- #707 @ 13,968: 25x14
-- #708 @ 356,599: 20x24
-- #709 @ 813,426: 18x15
-- #710 @ 793,700: 10x11
-- #711 @ 766,834: 13x27
-- #712 @ 58,150: 10x27
-- #713 @ 54,233: 27x29
-- #714 @ 130,62: 14x23
-- #715 @ 885,122: 11x25
-- #716 @ 543,280: 11x29
-- #717 @ 650,896: 20x20
-- #718 @ 228,601: 18x13
-- #719 @ 726,311: 18x13
-- #720 @ 352,898: 19x29
-- #721 @ 55,173: 27x27
-- #722 @ 731,530: 16x10
-- #723 @ 662,20: 26x26
-- #724 @ 951,648: 22x13
-- #725 @ 353,484: 22x27
-- #726 @ 802,863: 22x23
-- #727 @ 984,237: 12x24
-- #728 @ 541,739: 22x17
-- #729 @ 588,274: 14x29
-- #730 @ 854,718: 13x11
-- #731 @ 562,777: 17x26
-- #732 @ 789,493: 15x18
-- #733 @ 475,50: 27x25
-- #734 @ 794,394: 11x8
-- #735 @ 179,735: 27x12
-- #736 @ 27,375: 22x24
-- #737 @ 403,868: 29x27
-- #738 @ 5,166: 19x17
-- #739 @ 572,979: 15x19
-- #740 @ 400,707: 15x29
-- #741 @ 968,916: 14x24
-- #742 @ 404,184: 11x19
-- #743 @ 42,953: 25x11
-- #744 @ 955,779: 26x14
-- #745 @ 940,339: 19x27
-- #746 @ 868,644: 12x10
-- #747 @ 808,756: 11x11
-- #748 @ 462,20: 26x20
-- #749 @ 112,453: 12x27
-- #750 @ 839,883: 28x15
-- #751 @ 179,310: 21x28
-- #752 @ 809,425: 15x22
-- #753 @ 25,174: 17x20
-- #754 @ 248,915: 25x19
-- #755 @ 881,647: 17x29
-- #756 @ 339,312: 25x16
-- #757 @ 905,929: 11x20
-- #758 @ 22,508: 23x16
-- #759 @ 846,771: 20x16
-- #760 @ 119,608: 22x14
-- #761 @ 180,332: 13x29
-- #762 @ 443,42: 14x10
-- #763 @ 564,794: 10x3
-- #764 @ 845,74: 14x17
-- #765 @ 635,304: 27x14
-- #766 @ 297,965: 18x14
-- #767 @ 414,880: 28x24
-- #768 @ 135,13: 12x21
-- #769 @ 732,567: 22x23
-- #770 @ 616,582: 23x10
-- #771 @ 322,11: 21x13
-- #772 @ 540,806: 21x10
-- #773 @ 441,407: 15x10
-- #774 @ 729,773: 17x18
-- #775 @ 14,355: 14x26
-- #776 @ 624,122: 19x18
-- #777 @ 811,839: 10x26
-- #778 @ 316,182: 22x29
-- #779 @ 741,270: 12x18
-- #780 @ 931,842: 16x15
-- #781 @ 396,263: 16x23
-- #782 @ 868,207: 16x21
-- #783 @ 931,827: 10x14
-- #784 @ 600,804: 12x21
-- #785 @ 878,578: 12x12
-- #786 @ 398,120: 21x11
-- #787 @ 385,369: 23x20
-- #788 @ 671,162: 11x16
-- #789 @ 956,130: 15x29
-- #790 @ 564,735: 20x24
-- #791 @ 721,811: 12x21
-- #792 @ 895,187: 18x17
-- #793 @ 657,276: 21x13
-- #794 @ 1,444: 23x21
-- #795 @ 641,70: 13x17
-- #796 @ 33,979: 29x17
-- #797 @ 827,456: 17x16
-- #798 @ 809,906: 26x25
-- #799 @ 179,100: 20x11
-- #800 @ 164,909: 19x13
-- #801 @ 302,579: 24x14
-- #802 @ 963,692: 18x23
-- #803 @ 557,477: 24x26
-- #804 @ 609,937: 16x24
-- #805 @ 294,643: 24x12
-- #806 @ 707,295: 25x12
-- #807 @ 143,88: 28x26
-- #808 @ 120,701: 10x29
-- #809 @ 405,868: 22x13
-- #810 @ 438,706: 20x10
-- #811 @ 174,736: 18x10
-- #812 @ 553,652: 18x16
-- #813 @ 876,53: 26x22
-- #814 @ 756,271: 28x25
-- #815 @ 684,845: 13x14
-- #816 @ 29,527: 11x25
-- #817 @ 266,223: 13x11
-- #818 @ 48,460: 19x28
-- #819 @ 383,769: 25x16
-- #820 @ 807,339: 21x26
-- #821 @ 857,596: 23x18
-- #822 @ 270,914: 22x15
-- #823 @ 378,968: 16x29
-- #824 @ 179,831: 23x29
-- #825 @ 49,227: 20x16
-- #826 @ 879,720: 17x23
-- #827 @ 564,17: 29x18
-- #828 @ 754,144: 18x20
-- #829 @ 206,920: 19x25
-- #830 @ 455,501: 23x22
-- #831 @ 779,112: 15x11
-- #832 @ 40,476: 26x24
-- #833 @ 771,225: 23x26
-- #834 @ 358,682: 29x10
-- #835 @ 8,974: 22x19
-- #836 @ 18,744: 26x20
-- #837 @ 196,239: 18x13
-- #838 @ 895,312: 25x14
-- #839 @ 285,666: 26x23
-- #840 @ 14,868: 22x10
-- #841 @ 695,297: 18x26
-- #842 @ 949,341: 20x11
-- #843 @ 440,91: 25x23
-- #844 @ 887,688: 10x29
-- #845 @ 320,710: 21x16
-- #846 @ 598,156: 20x13
-- #847 @ 860,212: 29x19
-- #848 @ 728,316: 21x13
-- #849 @ 404,553: 23x29
-- #850 @ 556,424: 10x15
-- #851 @ 532,491: 21x12
-- #852 @ 735,29: 19x13
-- #853 @ 890,612: 19x12
-- #854 @ 338,834: 14x18
-- #855 @ 762,264: 16x16
-- #856 @ 961,496: 14x16
-- #857 @ 616,756: 11x15
-- #858 @ 40,948: 12x15
-- #859 @ 492,139: 23x25
-- #860 @ 866,70: 26x19
-- #861 @ 909,652: 14x15
-- #862 @ 68,740: 28x21
-- #863 @ 765,736: 20x22
-- #864 @ 687,856: 16x12
-- #865 @ 187,76: 25x27
-- #866 @ 104,162: 13x21
-- #867 @ 377,479: 13x27
-- #868 @ 768,553: 17x19
-- #869 @ 914,744: 11x22
-- #870 @ 338,152: 10x18
-- #871 @ 665,305: 12x12
-- #872 @ 401,702: 25x29
-- #873 @ 78,285: 26x26
-- #874 @ 931,369: 15x14
-- #875 @ 710,483: 17x28
-- #876 @ 228,717: 19x23
-- #877 @ 819,390: 19x28
-- #878 @ 797,673: 10x22
-- #879 @ 528,282: 25x15
-- #880 @ 887,703: 15x24
-- #881 @ 591,295: 22x10
-- #882 @ 892,777: 16x25
-- #883 @ 893,148: 28x21
-- #884 @ 477,897: 13x24
-- #885 @ 932,299: 17x21
-- #886 @ 190,120: 23x14
-- #887 @ 203,598: 27x18
-- #888 @ 803,934: 28x29
-- #889 @ 126,204: 15x26
-- #890 @ 388,379: 27x15
-- #891 @ 159,730: 11x21
-- #892 @ 185,638: 14x22
-- #893 @ 943,572: 26x14
-- #894 @ 585,740: 23x26
-- #895 @ 258,606: 27x28
-- #896 @ 329,66: 29x16
-- #897 @ 613,935: 14x29
-- #898 @ 947,156: 24x22
-- #899 @ 753,295: 25x23
-- #900 @ 763,591: 14x19
-- #901 @ 662,409: 20x22
-- #902 @ 627,213: 25x10
-- #903 @ 324,813: 7x6
-- #904 @ 275,695: 29x25
-- #905 @ 362,485: 24x14
-- #906 @ 965,761: 28x22
-- #907 @ 982,553: 12x10
-- #908 @ 335,787: 28x20
-- #909 @ 349,315: 19x25
-- #910 @ 226,62: 25x18
-- #911 @ 173,489: 27x26
-- #912 @ 213,204: 28x25
-- #913 @ 57,708: 28x19
-- #914 @ 623,612: 27x16
-- #915 @ 579,861: 12x19
-- #916 @ 895,0: 22x10
-- #917 @ 14,164: 22x15
-- #918 @ 68,284: 22x29
-- #919 @ 61,869: 16x18
-- #920 @ 318,539: 25x25
-- #921 @ 803,699: 16x11
-- #922 @ 620,713: 28x18
-- #923 @ 123,789: 21x28
-- #924 @ 171,279: 11x28
-- #925 @ 725,516: 25x22
-- #926 @ 202,570: 13x18
-- #927 @ 575,801: 21x29
-- #928 @ 715,485: 26x28
-- #929 @ 348,906: 15x20
-- #930 @ 683,680: 27x13
-- #931 @ 375,767: 19x27
-- #932 @ 347,915: 10x21
-- #933 @ 723,556: 27x26
-- #934 @ 737,291: 25x12
-- #935 @ 850,110: 13x18
-- #936 @ 132,326: 23x24
-- #937 @ 523,757: 27x13
-- #938 @ 747,720: 26x11
-- #939 @ 12,520: 19x13
-- #940 @ 868,576: 19x23
-- #941 @ 552,792: 19x12
-- #942 @ 18,620: 23x20
-- #943 @ 689,569: 16x22
-- #944 @ 518,316: 28x25
-- #945 @ 364,927: 21x13
-- #946 @ 119,116: 16x16
-- #947 @ 374,790: 10x12
-- #948 @ 53,165: 10x19
-- #949 @ 361,493: 16x26
-- #950 @ 400,102: 22x12
-- #951 @ 544,700: 23x15
-- #952 @ 461,339: 22x10
-- #953 @ 307,842: 27x25
-- #954 @ 299,77: 28x18
-- #955 @ 208,153: 22x21
-- #956 @ 206,710: 27x12
-- #957 @ 554,963: 29x25
-- #958 @ 160,786: 19x26
-- #959 @ 982,751: 11x29
-- #960 @ 121,29: 25x20
-- #961 @ 341,788: 17x19
-- #962 @ 288,537: 21x13
-- #963 @ 878,230: 10x17
-- #964 @ 704,222: 14x24
-- #965 @ 904,783: 29x13
-- #966 @ 270,357: 17x11
-- #967 @ 760,567: 14x26
-- #968 @ 368,585: 21x23
-- #969 @ 754,609: 11x12
-- #970 @ 16,372: 12x17
-- #971 @ 822,975: 16x21
-- #972 @ 738,125: 11x10
-- #973 @ 158,796: 24x15
-- #974 @ 151,814: 17x12
-- #975 @ 250,0: 24x22
-- #976 @ 430,78: 11x14
-- #977 @ 422,565: 26x29
-- #978 @ 328,573: 10x17
-- #979 @ 733,338: 12x19
-- #980 @ 759,44: 19x29
-- #981 @ 170,175: 16x14
-- #982 @ 766,135: 10x21
-- #983 @ 389,96: 28x20
-- #984 @ 972,848: 14x15
-- #985 @ 950,899: 13x28
-- #986 @ 391,980: 16x13
-- #987 @ 878,327: 12x13
-- #988 @ 812,922: 22x23
-- #989 @ 349,475: 19x19
-- #990 @ 498,152: 10x15
-- #991 @ 12,428: 13x29
-- #992 @ 801,799: 17x28
-- #993 @ 564,877: 18x18
-- #994 @ 553,733: 14x24
-- #995 @ 520,325: 24x15
-- #996 @ 73,267: 27x11
-- #997 @ 45,186: 23x21
-- #998 @ 514,531: 14x24
-- #999 @ 732,235: 18x10
-- #1000 @ 832,185: 15x22
-- #1001 @ 734,942: 11x24
-- #1002 @ 5,175: 10x24
-- #1003 @ 554,498: 14x16
-- #1004 @ 969,428: 22x19
-- #1005 @ 330,227: 13x20
-- #1006 @ 97,359: 28x27
-- #1007 @ 907,151: 28x26
-- #1008 @ 193,235: 10x16
-- #1009 @ 870,309: 22x15
-- #1010 @ 114,180: 11x11
-- #1011 @ 713,489: 19x19
-- #1012 @ 349,665: 26x11
-- #1013 @ 138,723: 12x26
-- #1014 @ 920,289: 11x19
-- #1015 @ 331,208: 28x27
-- #1016 @ 886,699: 19x12
-- #1017 @ 246,961: 14x22
-- #1018 @ 304,596: 12x12
-- #1019 @ 436,770: 14x22
-- #1020 @ 870,124: 19x13
-- #1021 @ 23,946: 24x13
-- #1022 @ 851,100: 13x16
-- #1023 @ 262,208: 19x21
-- #1024 @ 80,263: 25x19
-- #1025 @ 463,540: 11x19
-- #1026 @ 710,442: 29x25
-- #1027 @ 635,678: 20x13
-- #1028 @ 551,186: 17x21
-- #1029 @ 307,910: 10x29
-- #1030 @ 819,357: 11x16
-- #1031 @ 630,121: 10x27
-- #1032 @ 677,403: 23x27
-- #1033 @ 8,944: 12x19
-- #1034 @ 439,498: 21x10
-- #1035 @ 122,293: 24x28
-- #1036 @ 680,763: 15x11
-- #1037 @ 906,655: 26x25
-- #1038 @ 302,454: 23x10
-- #1039 @ 596,297: 3x5
-- #1040 @ 680,537: 28x10
-- #1041 @ 347,730: 21x12
-- #1042 @ 410,663: 17x24
-- #1043 @ 898,584: 24x6
-- #1044 @ 778,59: 24x10
-- #1045 @ 601,91: 16x13
-- #1046 @ 434,483: 24x14
-- #1047 @ 771,963: 17x10
-- #1048 @ 279,205: 26x11
-- #1049 @ 591,775: 3x12
-- #1050 @ 799,762: 23x27
-- #1051 @ 760,653: 17x15
-- #1052 @ 374,828: 24x26
-- #1053 @ 52,606: 28x27
-- #1054 @ 41,247: 27x26
-- #1055 @ 551,483: 19x15
-- #1056 @ 565,19: 14x11
-- #1057 @ 207,676: 11x10
-- #1058 @ 533,85: 11x24
-- #1059 @ 490,532: 17x21
-- #1060 @ 721,473: 12x17
-- #1061 @ 757,587: 15x10
-- #1062 @ 465,729: 23x18
-- #1063 @ 279,698: 17x3
-- #1064 @ 217,169: 14x18
-- #1065 @ 183,680: 23x14
-- #1066 @ 362,483: 26x19
-- #1067 @ 680,762: 17x22
-- #1068 @ 622,688: 23x20
-- #1069 @ 899,760: 15x26
-- #1070 @ 836,618: 11x28
-- #1071 @ 241,175: 13x14
-- #1072 @ 225,191: 16x19
-- #1073 @ 488,131: 12x28
-- #1074 @ 138,568: 17x27
-- #1075 @ 42,943: 22x25
-- #1076 @ 868,603: 24x27
-- #1077 @ 519,409: 28x28
-- #1078 @ 46,667: 16x20
-- #1079 @ 616,483: 13x5
-- #1080 @ 288,435: 20x22
-- #1081 @ 324,197: 27x22
-- #1082 @ 408,792: 12x18
-- #1083 @ 69,618: 20x23
-- #1084 @ 535,750: 28x12
-- #1085 @ 679,457: 16x14
-- #1086 @ 920,741: 24x15
-- #1087 @ 244,626: 18x10
-- #1088 @ 569,884: 11x15
-- #1089 @ 919,294: 15x17
-- #1090 @ 603,442: 22x16
-- #1091 @ 417,468: 24x28
-- #1092 @ 557,661: 10x14
-- #1093 @ 403,853: 13x22
-- #1094 @ 50,972: 12x16
-- #1095 @ 371,794: 14x12
-- #1096 @ 568,621: 24x14
-- #1097 @ 152,786: 14x29
-- #1098 @ 204,880: 14x22
-- #1099 @ 819,848: 14x7
-- #1100 @ 307,782: 22x21
-- #1101 @ 289,708: 24x14
-- #1102 @ 147,818: 24x21
-- #1103 @ 715,545: 26x19
-- #1104 @ 281,431: 17x11
-- #1105 @ 806,367: 19x12
-- #1106 @ 766,745: 18x23
-- #1107 @ 766,815: 27x14
-- #1108 @ 838,753: 22x28
-- #1109 @ 144,293: 29x29
-- #1110 @ 756,357: 14x25
-- #1111 @ 876,605: 12x9
-- #1112 @ 764,243: 12x26
-- #1113 @ 82,758: 18x19
-- #1114 @ 153,58: 11x20
-- #1115 @ 403,91: 12x24
-- #1116 @ 643,357: 27x28
-- #1117 @ 198,352: 22x29
-- #1118 @ 861,870: 16x15
-- #1119 @ 110,281: 20x22
-- #1120 @ 373,480: 13x16
-- #1121 @ 778,270: 23x20
-- #1122 @ 567,300: 23x19
-- #1123 @ 349,668: 27x24
-- #1124 @ 720,496: 29x28
-- #1125 @ 330,119: 22x14
-- #1126 @ 763,388: 22x16
-- #1127 @ 833,592: 10x21
-- #1128 @ 871,716: 26x27
-- #1129 @ 611,144: 10x15
-- #1130 @ 279,716: 17x12
-- #1131 @ 824,588: 12x25
-- #1132 @ 703,132: 21x11
-- #1133 @ 569,402: 22x26
-- #1134 @ 874,908: 28x24
-- #1135 @ 33,776: 28x22
-- #1136 @ 876,902: 14x19
-- #1137 @ 84,712: 19x24
-- #1138 @ 171,284: 10x14
-- #1139 @ 659,865: 23x20
-- #1140 @ 133,924: 26x17
-- #1141 @ 261,41: 19x13
-- #1142 @ 32,351: 11x17
-- #1143 @ 777,337: 22x23
-- #1144 @ 469,342: 16x12
-- #1145 @ 258,611: 18x26
-- #1146 @ 13,405: 10x10
-- #1147 @ 185,653: 24x14
-- #1148 @ 349,289: 29x28
-- #1149 @ 771,653: 18x22
-- #1150 @ 432,612: 11x12
-- #1151 @ 421,525: 19x29
-- #1152 @ 39,601: 12x20
-- #1153 @ 315,230: 12x21
-- #1154 @ 574,739: 11x10
-- #1155 @ 335,779: 20x13
-- #1156 @ 874,949: 19x28
-- #1157 @ 0,823: 16x15
-- #1158 @ 6,698: 29x18
-- #1159 @ 351,514: 18x24
-- #1160 @ 669,551: 21x14
-- #1161 @ 893,799: 25x12
-- #1162 @ 51,116: 11x20
-- #1163 @ 34,184: 26x22
-- #1164 @ 669,868: 12x11
-- #1165 @ 10,601: 3x9
-- #1166 @ 460,805: 20x13
-- #1167 @ 357,750: 21x20
-- #1168 @ 70,600: 10x16
-- #1169 @ 397,188: 20x22
-- #1170 @ 604,163: 14x27
-- #1171 @ 696,299: 10x21
-- #1172 @ 839,455: 29x28
-- #1173 @ 808,781: 14x26
-- #1174 @ 565,803: 10x10
-- #1175 @ 131,153: 24x19
-- #1176 @ 347,824: 22x26
-- #1177 @ 583,296: 15x15
-- #1178 @ 777,951: 15x24
-- #1179 @ 211,122: 23x17
-- #1180 @ 804,576: 23x14
-- #1181 @ 401,766: 28x28
-- #1182 @ 816,817: 17x28
-- #1183 @ 499,278: 11x19
-- #1184 @ 320,515: 18x26
-- #1185 @ 458,711: 26x20
-- #1186 @ 496,612: 15x28
-- #1187 @ 783,795: 10x21
-- #1188 @ 837,681: 14x26
-- #1189 @ 310,762: 17x25
-- #1190 @ 551,197: 12x10
-- #1191 @ 889,898: 22x13
-- #1192 @ 750,733: 29x19
-- #1193 @ 596,925: 18x10
-- #1194 @ 194,684: 6x4
-- #1195 @ 202,344: 26x21
-- #1196 @ 99,809: 15x10
-- #1197 @ 606,256: 18x21
-- #1198 @ 307,190: 25x15
-- #1199 @ 442,34: 29x16
-- #1200 @ 391,380: 14x20
-- #1201 @ 853,158: 17x23
-- #1202 @ 209,658: 22x24
-- #1203 @ 182,691: 4x4
-- #1204 @ 675,816: 18x26
-- #1205 @ 923,379: 27x28
-- #1206 @ 54,315: 9x8
-- #1207 @ 230,892: 24x17
-- #1208 @ 819,479: 24x24
-- #1209 @ 516,580: 26x22
-- #1210 @ 270,218: 25x17
-- #1211 @ 981,377: 15x20
-- #1212 @ 788,499: 11x10
-- #1213 @ 623,343: 28x18
-- #1214 @ 594,489: 23x15
-- #1215 @ 434,678: 17x10
-- #1216 @ 555,522: 13x16
-- #1217 @ 707,534: 29x22
-- #1218 @ 381,465: 22x20
-- #1219 @ 93,173: 19x28
-- #1220 @ 213,513: 17x11
-- #1221 @ 234,61: 13x27
-- #1222 @ 508,577: 18x27
-- #1223 @ 718,122: 17x11
-- #1224 @ 274,577: 29x12
-- #1225 @ 372,829: 19x18
-- #1226 @ 344,263: 14x20
-- #1227 @ 749,280: 14x23
-- #1228 @ 744,488: 13x18
-- #1229 @ 777,204: 26x20
-- #1230 @ 460,32: 29x12
-- #1231 @ 253,151: 13x20
-- #1232 @ 950,198: 27x11
-- #1233 @ 307,137: 14x18
-- #1234 @ 554,332: 29x11
-- #1235 @ 770,318: 15x22
-- #1236 @ 331,38: 29x20
-- #1237 @ 779,493: 22x16
-- #1238 @ 739,335: 21x23
-- #1239 @ 396,570: 21x11
-- #1240 @ 354,309: 11x10
-- #1241 @ 809,320: 29x24
-- #1242 @ 959,879: 20x20
-- #1243 @ 489,346: 14x29
-- #1244 @ 468,179: 22x27
-- #1245 @ 794,21: 23x18
-- #1246 @ 51,708: 14x28
-- #1247 @ 913,448: 17x19
-- #1248 @ 810,787: 20x12
-- #1249 @ 896,578: 29x24
-- #1250 @ 948,562: 12x19
-- #1251 @ 756,410: 24x13
-- #1252 @ 373,589: 12x7
-- #1253 @ 474,224: 22x19
-- #1254 @ 127,222: 23x14
-- #1255 @ 422,317: 19x16
-- #1256 @ 372,250: 20x28
-- #1257 @ 932,598: 21x12
-- #1258 @ 529,151: 20x15
-- #1259 @ 422,441: 14x22
-- #1260 @ 751,761: 21x11
-- #1261 @ 956,817: 25x29
-- #1262 @ 622,418: 19x27
-- #1263 @ 490,38: 29x15
-- #1264 @ 525,310: 23x17
-- #1265 @ 2,610: 29x21
-- #1266 @ 873,517: 12x23
-- #1267 @ 565,594: 25x16
-- #1268 @ 306,500: 17x18
-- #1269 @ 616,480: 16x13
-- #1270 @ 742,971: 20x21
-- #1271 @ 633,573: 24x14
-- #1272 @ 399,11: 25x15
-- #1273 @ 334,186: 23x16
-- #1274 @ 13,754: 26x22
-- #1275 @ 500,784: 17x24
-- #1276 @ 316,811: 20x17
-- #1277 @ 128,842: 15x22
-- #1278 @ 558,515: 11x12
-- #1279 @ 610,262: 4x7
-- #1280 @ 499,321: 26x29
-- #1281 @ 178,367: 15x14
-- #1282 @ 826,493: 19x27
-- #1283 @ 462,871: 26x22
-- #1284 @ 91,889: 14x29
-- #1285 @ 157,746: 26x12
-- #1286 @ 799,339: 21x23
-- #1287 @ 221,661: 21x20
-- #1288 @ 697,551: 28x10
-- #1289 @ 241,530: 17x26
-- #1290 @ 72,249: 18x15
-- #1291 @ 948,549: 29x12
-- #1292 @ 564,689: 29x14
-- #1293 @ 714,265: 27x15
-- #1294 @ 629,198: 3x8
-- #1295 @ 766,155: 12x12
-- #1296 @ 590,742: 13x21
-- #1297 @ 125,605: 29x26
-- #1298 @ 285,461: 21x26
-- #1299 @ 416,16: 11x22
-- #1300 @ 237,466: 18x11
-- #1301 @ 784,394: 23x19
-- #1302 @ 953,368: 27x25
-- #1303 @ 375,976: 20x12
-- #1304 @ 199,656: 25x11
-- #1305 @ 582,121: 29x11
-- #1306 @ 63,620: 29x17
-- #1307 @ 846,612: 19x24
-- #1308 @ 588,799: 11x16
-- #1309 @ 209,633: 21x23
-- #1310 @ 120,453: 20x13
-- #1311 @ 566,915: 20x11
-- #1312 @ 917,666: 23x27
-- #1313 @ 439,905: 22x12
-- #1314 @ 605,131: 19x25
-- #1315 @ 603,30: 16x20
-- #1316 @ 477,183: 26x25
-- #1317 @ 638,66: 21x27
-- #1318 @ 729,959: 12x15
-- #1319 @ 119,379: 20x11
-- #1320 @ 543,281: 25x27
-- #1321 @ 82,728: 10x11
-- #1322 @ 336,557: 29x27
-- #1323 @ 277,314: 22x14
-- #1324 @ 810,286: 28x21
-- #1325 @ 164,397: 18x17
-- #1326 @ 105,190: 15x17
-- #1327 @ 250,357: 18x25
-- #1328 @ 818,347: 12x18
-- #1329 @ 525,299: 11x29
-- #1330 @ 944,627: 12x23
-- #1331 @ 599,609: 14x19
-- #1332 @ 756,125: 26x18
-- #1333 @ 454,881: 19x18
-- #1334 @ 880,142: 18x20
-- #1335 @ 310,590: 10x11
-- #1336 @ 239,879: 27x19
-- #1337 @ 280,327: 24x19
-- """

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { i : Int
  , claims : List Claim
  , fabric : Dict (Int, Int) Bool
  }

readInput : String -> List Claim
readInput input =
  String.split "\n" input
    |> List.filter (\s -> not (String.isEmpty s))
    |> List.map (makeClaim)

makeClaim : String -> Claim
makeClaim str =
  case Parser.run parseClaim str of
    Ok expr ->
      expr
    Err err -> 
      Debug.todo "Input is always perfect, QED: "

parseClaim : Parser Claim
parseClaim =
  succeed Claim
    |. symbol "#"
    |= int
    |. symbol " @ "
    |= int
    |. symbol ","
    |= int
    |. symbol ": "
    |= int
    |. symbol "x"
    |= int

matrix : Int -> Int -> Bool -> Dict (Int, Int) Bool
matrix w h v = 
  case (w, h, v) of
    (1, 1, _) -> Dict.fromList [ ((1, 1), v) ]
    (1, _, _) -> matrix 1 (h - 1) v
                   |> Dict.insert (1, h) v
    (_, _, _) -> matrix (w - 1) h v 
                   |> Dict.insert (w, 1) v

-- matrix 1 1 v = Dict.fromList [ ((1, 1), v) ]
-- matrix 1 h v = 
--   matrix 1 (h - 1) v 
--     |> Dict.insert (1, h) v
-- matrix w 1 v = Dict.fromList [ ((1, 1), v) ]
--   matrix (w - 1) h v 
--     |> Dict.insert (w, 1) v
  

init : Model
init =
  { i = 0
  , claims = readInput myInput
  , fabric = matrix 1000 1000 False
  }


-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  model

-- VIEW
renderClaim : Claim -> Html Msg
renderClaim claim =
  div []
    [ span [ class "badge", style "background-color" "salmon" ] [ text (claim.id |> String.fromInt) ]
    , span [ class "badge", style "background-color" "cyan" ] [ text (claim.x |> String.fromInt) ]
    , span [ class "badge", style "background-color" "blue" ] [ text (claim.y |> String.fromInt) ]
    , span [ class "badge", style "background-color" "yellow" ] [ text (claim.w |> String.fromInt) ]
    , span [ class "badge", style "background-color" "bisque" ] [ text (claim.h |> String.fromInt) ]
    ]
view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    -- , div [] [ text (input) ]
    , case model.claims of 
      Loading ->
        text "Loading"
      Success claims ->
        div [] (List.map renderClaim claims)
    , button [ onClick Increment ] [ text "+" ]
    ]
