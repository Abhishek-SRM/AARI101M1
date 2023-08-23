**Free
// %CSTD===========================================================*
// * Application. : ARC        PRD:ARCAD IBMi part                 *
// * Composant. . : AARI101M9                     Type: SQLRPGLE   *
// *===============================================================*
// * Sous-système : IUN IUNIT Repository/Analyzer                  *
// * Fonction . . :                                                *
// * Sous-fonction:                                                *
// *%S=============================================================*
// * Description des fonctionnalités:                              *
// *                                                               *
// *                                                               *
// *                                                               *
// *%E=============================================================*
// * AUTEUR:    ABSINGH    01/11/2022 09:53  22.03.00              *
// * MODIFS: ** ABSINGH    01/11/2022 09:53  22.03.00 FM 22/00358  *
// *           Rewrite the AARI011G1 program                       *
// %ECSTD==========================================================*

Dcl-f AARIFFDL2 Keyed usropn usage(*input:*output);
Dcl-f AARIOBJL3 Keyed usropn usage(*input:*output);
Dcl-f AARIWFDF1 Keyed usropn usage(*input:*output);
Dcl-f AARIWFDL1 Keyed        usage(*input:*output)
                             Rename(AARIWFDFF :WFDLF1R);
Dcl-f AARISMBL2 keyed usropn;
Dcl-f AARILBSL1 keyed usropn;
Dcl-f AARIFFIF1 Keyed;
Dcl-f AARIVRIF1 keyed usropn usage(*input:*output:*update);
Dcl-f AARIVRIL1 keyed usropn rename(AARIVRIFF:ARIVRL1);
Dcl-f AARIPGIF1 keyed usropn usage(*input:*output:*update);


Dcl-Ds Ds_wsrc;
  s_cLngKwd        ucs2(500)  pos(1);
  s_cSrcDta        ucs2(158)  pos(1);
  s_cSrcCode       ucs2(81)   pos(1);
  s_cTags          ucs2(5)    pos(1);
  s_cSpec          ucs2(1)    pos(11);
  s_cPos07         ucs2(1)    pos(13);
  s_cFileName      ucs2(10)   pos(13);
  s_cFieldName     ucs2(14)   pos(13);
  s_cDeclareType   ucs2(4)    pos(43);
  s_cDefType       ucs2(2)    pos(47);
  s_cOpcode        ucs2(10)   pos(51);
  s_cFactor2       ucs2(14)   pos(71);
  s_cFactor1       ucs2(14)   pos(23);
  s_cResult        ucs2(14)   pos(99);
  s_cFieldLen      ucs2(5)    pos(127);
  s_cDecPosCspc    ucs2(2)    pos(137);
  s_cFldLen        ucs2(7)    pos(65);
  s_cDevice        ucs2(7)    pos(71);
  s_cFldTyp        ucs2(1)    pos(79);
  s_cDecPos        ucs2(2)    pos(81);
  s_cSource        ucs2(74)   pos(13);
  s_cSrcKwds       ucs2(37)   pos(87);
  s_cComment       ucs2(20)   pos(161);
  s_cCPYMBRF       ucs2(10)   pos(201);
  s_cCPYSRCF       ucs2(10)   pos(221);
  s_cCPYSLIB       ucs2(10)   pos(241);
End-Ds;


//*Data Structure To Store Sequence And RRN
Dcl-Ds Ds_wRRN;
  s_nSrcSeq     zoned(6:2);
  s_nSrcDat     zoned(6:0);
  s_aSrcDta     char(158);
  s_aCpyMbr     char(10)    overlay(s_aSrcDta :101);
  s_aCpyFil     char(10)    overlay(s_aSrcDta :111);
  s_aCpyLib     char(10)    overlay(s_aSrcDta :121);
  s_nOrgSrcRRN  zoned(7:0)  overlay(s_aSrcDta :131);
  s_nOrgSrcSeq  zoned(7:2)  overlay(s_aSrcDta :138);
  s_nCpybkRRN   zoned(7:0)  overlay(s_aSrcDta :145);
  s_nCpybkSeq   zoned(7:2)  overlay(s_aSrcDta :152);
End-Ds ;

//*Data Structure To Store File Information
Dcl-Ds  Ds_wFilwrt qualified;
  s_IRepId            packed(9:0);
  s_ISrcId            packed(9:0);
  s_IPrcId            packed(9:0);
  s_cFilnam           Ucs2(10);
  s_cExtFilNam        Ucs2(10);
  s_cFilFmt           Ucs2(1);
  s_cOrgRf            Ucs2(10)   dim(999);
  s_cRnmRf            Ucs2(10)   dim(999);
  s_cPrefix           Ucs2(10);
  s_nPfxPos           packed(5:0);
  s_cUsage            Ucs2(1);
  s_cAlias            Ucs2(1);
  s_cDatFmt           Ucs2(5);
  s_cDevice           Ucs2(7);
  s_cIgnRf            Ucs2(10)   dim(999);
  s_cIncRf            Ucs2(10)   dim(999);
  s_cSfile            Ucs2(10)   dim(999);
  s_nRfLen            packed(5:0);
  s_wbgn              likeds(ds_wRRN);
  s_wend              likeds(ds_wRRN);
End-Ds;


//* Constants ------------------------------------
dcl-c c_cQut     %UCS2('''') ;
dcl-c c_cLPa     %Ucs2('(');
dcl-c c_cRPa     %Ucs2(')');
dcl-c c_cAst     %Ucs2('*');
dcl-c c_cCln     %Ucs2(':');
dcl-c c_cBlk     %Ucs2(' ');

//* File Spec Keywords Constants
dcl-c c_cDclF    %ucs2('DCL-F');
dcl-c c_cRename  %Ucs2('RENAME') ;
dcl-c c_cPrefix  %Ucs2('PREFIX') ;
dcl-c c_cInclude %Ucs2('INCLUDE');
dcl-c c_cIgnore  %Ucs2('IGNORE') ;
dcl-c c_cSfile   %Ucs2('SFILE')  ;
dcl-c c_cDatfmt  %Ucs2('DATFMT') ;
dcl-c c_cLikFil  %Ucs2('LIKEFILE');
dcl-c c_cUsage   %Ucs2('USAGE')  ;
dcl-c c_cPrntr   %Ucs2('PRINTER');
dcl-c c_cWrkstn  %Ucs2('WORKSTN');
dcl-c c_cDisk    %Ucs2('DISK')   ;
dcl-c c_cSize    %Ucs2('%SIZE')  ;
dcl-c c_cElem    %Ucs2('%ELEM')  ;
dcl-c c_cLen     %Ucs2('%LEN')   ;

//*Terminal Array Indexes
dcl-s i_nconskwd  packed(5:0) Inz(1);    //*Constant Index

//LikeFile Processing Ds After All Processing Done
Dcl-Ds  Ds_wLikKwd ;
  s_cLikfil     Ucs2(10);   // LikeFileName
  s_cOrgfil     Ucs2(10);   // File Name
End-Ds;

//Global Ds array For Storing Constant Variable
Dcl-Ds  Ds_wConsKwd   Dim(500) qualified;
  s_cConsnam    Ucs2(10);   // Constant Name
  s_cBIf        Ucs2(10);   // BIF
  s_cFilname    Ucs2(10);   // File Name
End-Ds;

//*====================================================
//*Prototype For Process DCL-F Spec
//*====================================================
Dcl-Pr yPrcFreeFile;
  e_IRepId packed(9:0);
  e_IMbrId packed(9:0);
  e_IPrcId packed(9:0);
  e_WSrc likeds(DS_WSRC);
  e_wRRN likeds(DS_WRRN);
End-Pr;

//*====================================================
//*Prototype to Write File Field Information
//*====================================================

Dcl-Pr YWrtFileFld;
  pl_filedtl likeds(Ds_wFilwrt);
  pl_fileRRn likeds(Ds_WRRN);
  pl_fileSrc likeds(Ds_wSRC);
End-Pr;

//*====================================================
//*Prototype For Process  F Spec
//*====================================================
Dcl-Pr yPrcFixFile;
  e_IRepId  packed(9:0);
  e_ISrcId  packed(9:0);
  e_IPrcId  packed(9:0);
  e_WSrc    likeds(DS_WSRC);
  e_wRRN    likeds(DS_WRRN);
End-Pr;

//*=====================================================
//Prototype definition to fetch variable Id from name
//*=====================================================
dcl-pr yGetVarID packed(9:0);
  e_iRepId packed(9:0);
  e_JVarName ucs2(50);
end-pr;

//*=====================================================
//Prototype definition to fetch Keyword Id
//*=====================================================
Dcl-pr nGetKeywordId  packed(9:0);
  e_IRepId    packed(9:0);
  e_cKeyWord  Ucs2(500);
End-pr;

//*=====================================================
//Prototype definition to Get Object Id
//*=====================================================
Dcl-pr yGetobjId  packed(9:0);
  e_IRepId    packed(9:0);
  e_cObjName  Ucs2(10);
End-pr;

//*=====================================================
//Prototype definition of Program to Fetch Keyword ID
//*=====================================================

Dcl-pr  YAARI022R1   ExtPgm('AARI022R1');
  e_NRepId      packed(9:0);
  e_CKeyWrd     ucs2(500);
  e_NKeyWrdId   packed(9:0);
End-pr;

//*=====================================================
//Procedure To Process File Keywords Eg. SFILE , RENAME
//*=====================================================
Dcl-Proc yPrcFreeFile  Export;
  Dcl-Pi yPrcFreeFile;
    e_IRepId packed(9:0);
    e_IMbrId packed(9:0);
    e_IPrcId packed(9:0);
    e_WSrc likeds(DS_WSRC);
    e_wRRN   likeds(DS_WRRN);
  End-Pi ;

  dcl-s l_ckwdval    Ucs2(10) dim(99);  //*Local Kwd Value Array
  dcl-s l_nkwdval    packed(5:0);       //*Index array
  dcl-s l_nPos1      packed(5:0);
  dcl-s l_nPos2     packed(5:0);
  dcl-s l_nPos3     packed(5:0);
//*Local String For Processing  Keywords
  dcl-s l_cString    ucs2(500);        //*Source String
  dcl-s l_cwrkStr    ucs2(500);        //*Source String
  dcl-s l_cUsage     Ucs2(1);           //*Usage
  dcl-s l_cDevice    Ucs2(7);           //*Device
  dcl-s l_cFilFmt    Ucs2(1);           //*FileFormat
  dcl-s l_nRfLen     packed(5:0);       //*Record Length
  dcl-ds lDs_wLikKwd LikeDs(Ds_wLikKwd);
//*Initially Set ds_wFilwrt to To  *Blanks
  Clear ds_wFilwrt ;
  Clear lDs_wLikKwd;
//*DataStrucure Field
  Ds_wFilwrt.s_IRepId  =  e_IRepId;               //Repository Id
  Ds_wFilwrt.s_ISrcId  =  e_IMbrId;               //Member Id
  Ds_wFilwrt.s_IPrcId  =  e_IPrcId;               //Member Id

//*Copy Source String To Local Source String
  l_cString  = e_WSrc.s_cLngKwd;
//*Scan And Get The Starting Position Of DCL-F
  l_nPos1 = %Scan(c_cDclF: l_cString);
//*Get First Index Of File Name After DCL-F
  l_nPos1 = %Check(c_cBlk:l_cString:l_nPos1+6) ;
//*Get The Blank Pos After First Index Of File
  l_nPos2 = %Scan(c_cBlk:l_cString:l_nPos1);
//*------- File Name -------------------*
  Ds_wFilwrt.s_cFilnam = %Subst(l_cString:l_nPos1:l_nPos2-l_nPos1);

//* Extract String After File Name
  l_cString = %Subst(l_cString:l_nPos2);
//*-- Search LIKEFILE keyword --------------------------------*
  If %Scan(c_cLikFil :l_cString ) > 0;
    yPrcKwds(l_cString : c_cLikFil: l_ckwdval : l_nkwdval);
    If l_nKwdVal > 1;
        lDs_wLikKwd.s_cOrgfil =  Ds_wFilwrt.s_cFilnam;
        lDs_wLikKwd.s_cLikfil =  l_ckwdval(1);
        YPrcLikeFile(e_IRepId:e_IMbrId: e_wRRN : lDs_wLikKwd);
     //Store File Name And Its Like FileName Into Ds And Return
        Return;
    EndIf;
  EndIf;

//*-- Get External File Name Via Cursor ----------*
  YGetPgmInf(Ds_wFilwrt.s_cfilnam:Ds_wFilwrt.s_cextfilnam);

//*-- Search RENAME keyword --------------------------------*
  If %Scan(c_cRename :l_cString ) > 0;
    yPrcKwds(l_cString : c_cRename: l_ckwdval : l_nkwdval);
    If l_nKwdVal > 1;
      //If there are multiple Rename kwds, they all will be loaded in l_ckwdval array
      l_nPos2 = 1;
      For l_nPos1 = 1 To l_nkwdval - 1;
        //Process eac rename pair (i.e. original and renamed record format)
        Ds_wFilwrt.s_cOrgRf(l_nPos2) = l_ckwdval(l_nPos1);
        l_nPos1  += 1;
        //Load Renamed Record Format Name
        Ds_wFilwrt.s_cRnmRf(l_nPos2) = l_ckwdval(l_nPos1);
        l_npos2 += 1;
      EndFor;
    EndIf;
  EndIf;

//*-- Search PREFIX keyword --------------------------------*
  If %Scan(c_cPrefix  : l_cString ) > 0;
    yPrcKwds(l_cString :c_cPrefix :l_ckwdval : l_nkwdval);
    If l_nkwdval > 1;
      //Load prefix Name In Ds Field
      Ds_wFilwrt.s_cPrefix = l_ckwdval(1);
      //If Prefix Position Non Blank then Store
      If l_ckwdval(2) <> *Blanks;
        Ds_wFilwrt.s_nPfxPos = %dec(%char(l_ckwdval(2)):3:0);
      Endif;
      l_nPos1 = %scan(c_cQut: Ds_wFilwrt.s_cPrefix);
      If l_nPos1 > *Zeros;
        l_nPos2 = %Scan(c_cQut:Ds_wFilwrt.s_cPrefix : l_nPos1+1);
        If l_nPos2 > *Zeros;
           l_nPos3 = l_nPos2 - l_nPos1 ;
           If l_nPos3 > 1 ;
             Ds_wFilwrt.s_cPrefix = %Subst(Ds_wFilwrt.s_cPrefix :
                                2 : l_nPos2 - 2 ) ;
           Else;
             Ds_wFilwrt.s_cPrefix  = *Blanks;
           EndIf;
        EndIf;
      EndIf;
    EndIf;
  EndIf;

//*-- Search INCLUDE keyword --------------------------------*
  If %Scan(c_cInclude : l_cString ) > 0;
    yPrcKwds(l_cString : c_cInclude : l_ckwdval : l_nkwdval);
    If l_nkwdval > 1 ;
      For l_nPos1 = 1 To l_nkwdval - 1;
        //Load all Include Record Format  Into Array
        Ds_wFilwrt.s_cIncRf(l_nPos1) = l_ckwdval(l_nPos1);
      EndFor;
    EndIf;
  EndIf;

//*-- Search IGNORE  keyword --------------------------------*
  If %Scan(c_cIgnore : l_cString ) > 0;
    yPrcKwds(l_cString : c_cIgnore : l_ckwdval : l_nkwdval);
    If l_nkwdval > 1 ;
      For l_nPos1 = 1 To l_nkwdval - 1;
        Ds_wFilwrt.s_cIgnRf(l_nPos1) = l_ckwdval(l_nPos1);
      EndFor;
    EndIf;
  EndIf;

//*-- Search SFILE   keyword --------------------------------*
  If %Scan(c_cSfile : l_cString ) > 0;
    yPrcKwds(l_cString : c_cSfile : l_ckwdval : l_nkwdval);
    If l_nkwdval > 1 ;
      l_nPos2 = 1;
      For l_nPos1 = 1 To l_nkwdval - 1 by 2;
        //Load all SFILE Record Format
        Ds_wFilwrt.s_cSfile(l_nPos2) = l_ckwdval(l_nPos1);
        l_nPos2 += 1 ;
      EndFor;
    EndIf;
  EndIf;

//*-- Search DATFMT  keyword --------------------------------*
  If %Scan(c_cDatfmt :l_cString ) > 0;
    yPrcKwds(l_cString : c_cDatfmt : l_ckwdval : l_nkwdval);
    If l_nkwdval > 1 ;
      For l_nPos1 = 1 To l_nkwdval - 1;
        //Store Date Format
        Ds_wFilwrt.s_cDatFmt  = l_ckwdval(l_nPos1);
      EndFor;
    EndIf;
  EndIf;

//*-- Search ALIAS   keyword --------------------------------*
  Ds_wFilwrt.s_cAlias = %Ucs2('N');
  //If Alias With Blanks
  If %Scan(%Ucs2(' ALIAS '):l_cString)> 0 ;
    l_nPos1 = %Scan(%Ucs2(' ALIAS '):l_cString);
    Dow l_nPos1 > 0;
      //Valid Keyword If After Alias First Character Is Not : Or )
      l_cwrkStr = %Trim(%Subst(l_cString:l_nPos1+ 6));
      If %Subst(l_cwrkStr:1:1) <> c_cCln And
        %Subst(l_cwrkStr:1:1) <> c_cRpa;
        Ds_wFilwrt.s_cAlias = %Ucs2('Y');
        leave;
      Endif;
      l_nPos1 = %Scan(%Ucs2(' ALIAS '):l_cString : l_nPos1 + 1);
    EndDo;
  EndIf;

//*-- Set Device FileFmt Usage RecfFmtLen -------------*
//*--Scan USAGE PRINTER WORKSTN DISK ------------------*
  If %Scan(c_cUsage:l_cString ) > 0 or
    %Scan(c_cPrntr :l_cString)  > 0 or
    %Scan(c_cWrkstn:l_cString ) > 0 or
    %Scan(c_cDisk  :l_cString)  > 0;
//*Process These Keywords
    yPrcFile(l_cString:l_cUsage:l_cDevice:l_cFilFmt:l_nRfLen);
      Ds_wFilwrt.s_cUsage  =  l_cUsage;
      Ds_wFilwrt.s_cDevice =  l_cDevice;
      Ds_wFilwrt.s_cFilFmt =  l_cFilFmt;
      If l_nRfLen <> *zeros;
        Ds_wFilwrt.s_nRfLen  =  l_nRfLen;
      EndIf;
  Else;
    //Default values
    Ds_wFilwrt.s_cUsage  = %Ucs2('I');      //Default Usage
    Ds_wFilwrt.s_cDevice = %Ucs2('DISK');   //Default Device
    Ds_wFilwrt.s_cFilFmt = %Ucs2('E');      //Deafault FileFmt
  EndIf;

  if e_wRRn.s_nSrcSeq = *zeros;
    ds_wfilwrt.s_wbgn = e_wRRn;
    ds_wfilwrt.s_wend = e_wRRn;
  else;
    ds_wfilwrt.s_wend = e_wrrn;
  endif;

  YWrtFileFld(Ds_wFilwrt:e_wRRn:e_wSrc);
  *inlr = *on;
End-Proc;

//-------------------------------------------------------*
//Process Device Usage FileFmt Record Legth
//-------------------------------------------------------*
Dcl-Proc yPrcFile;
  Dcl-Pi yPrcFile;
    e_cString      ucs2(500);
    e_cUsage       Ucs2(1);
    e_cDevice      Ucs2(7);
    e_cFilFmt      Ucs2(1);
    e_nRfLen       packed(5:0);
  End-Pi;
  dcl-s l_cKeyword  Ucs2(500);
  dcl-s l_lok  ind ;
  dcl-s l_nPos1     packed(5:0);
  dcl-s l_nPos2     packed(5:0);
  dcl-s l_nPos3     packed(5:0);
//*-- Search PRINTER keyword --------------------------------*
  If %Scan(c_cPrntr :e_cString) > 0 ;
    //*Chek Valid Device Name
    yValidateDevice(e_cString:c_cPrntr:l_lok);
    //*If Device Keyword Is Valid Then Process
    If l_lok = *On;
      //*Set Device Name
      e_cDevice = c_cPrntr;
      //*Check File Program Described Or Externally Described
      ychkpgmdesc(e_cString :c_cPrntr:e_cFilFmt : e_nRfLen);
      //*Check File Usage Mode Eg. *input ....
      Exsr Xl_Usage ;
    EndIf;
  EndIf;

//*-- Search WORKSTN keyword --------------------------------*
  If %Scan(c_cWrkstn : e_cString ) >0;
    //*Chek Valid Device Name
    yValidateDevice(e_cString:c_cWrkstn:l_lok);
    //*If Device Keyword Is Valid Then Process
    If l_lok = *On;
      //*Set Device Name
      e_cDevice = c_cWrkstn;
      //*Check File Program Described Or Externally Described
      ychkpgmdesc(e_cString :c_cWrkstn :e_cFilFmt : e_nRfLen);
      //*Check File Usage Mode Eg. *input ....
      Exsr Xl_Usage ;
    EndIf;
  EndIf;

//*-- Search DISK keyword --------------------------------*
  If %Scan(c_cDisk :e_cString) > 0 ;
    //*Chek Valid Device Name
    yValidateDevice(e_cString:c_cDisk :l_lok);
    //*If Device Keyword Is Valid Then Process
    If l_lok = *On;
      //*Set Device Name
      e_cDevice = c_cDisk; //*Chek Valid Device Name
      //*Check File Program Described Or Externally Described
      ychkpgmdesc(e_cString :c_cDisk :e_cFilFmt : e_nRfLen);
      //*Check File Usage Mode Eg. *input ....
      Exsr Xl_Usage ;
    EndIf;
  EndIf;

//=======================================================*
//*-- If Valid Device Not Found Then  -------------------*
//*-- If Usage Not Set It Means Valid Device Not Found --*
//=======================================================*
  If e_cUsage = %ucs2(' ');
    //*Set Default File Format
    e_cFilFmt = %ucs2('E');  //dcl-f tstpf1 usage(*input:*output);
    //*Set Default Device
    e_cDevice = c_cDisk;
    //If Usage Keyword Exist
    If %Scan(c_cUsage  : e_cString)> 0;
      //*Check File Usage Mode Eg. *input ....
      ExSr Xl_Usage;
    Else;
     //If Usage Not There Then Default Usage
      e_cUsage = %ucs2('I');
    EndIf;
  EndIf;

//*-- Set Usage of Device --------------------------------*
  BegSr Xl_Usage;
    l_lok = *Off;
    l_cKeyword = e_cString ;
    //Scan Usage in the String
    l_nPos1 = %Scan(c_cUsage  : l_cKeyword);
    //Loop For Taking Valid Usage Keyword
    Dow l_nPos1 > 0;
      l_cKeyword = %TrimL(%Subst(l_cKeyword:l_nPos1+5));
      If %Subst(l_cKeyword:1:1) = c_cLpa;
        //If Valid Keyword Found Then Indicator *on And Leave
        l_lok = *On;
        leave;
      EndIf;
      l_nPos1 = %Scan(c_cUsage  : l_cKeyword);
    EndDo;
    //If Valid 'USAGE' Found
    //If Keyword Founded And Execute Loop
    If l_lok = *On;
      If %Subst(l_cKeyword:1:1) = c_cLpa;
        //Scan Left Parenthesis '('
        l_nPos1 = %Scan(c_cLpa:l_cKeyword);
        If l_nPos1 > 0;
          //Scan Right Parenthesis ')'
          l_nPos2 = %Scan(c_cRpa:l_cKeyword :l_nPos1);
          If l_nPos2 > 0;
            //Scan Colon ':' For usage(*update:*output:*delete)'
            l_nPos3 = %Scan(c_cCln:l_cKeyword :l_nPos1);
            //If Colon ':' Found
            If l_nPos3 > 0 And l_nPos3 <  l_nPos2;
              //If Usage(*INPUT:*OUTPUT) Occured
              If %Scan(%ucs2('*INPUT'):l_cKeyword :l_nPos1) > 0 and
                %Scan(%ucs2('*OUTPUT'):l_cKeyword :l_nPos1) > 0;
                 e_cUsage = %Ucs2('I');
              Else;
                //'U' Used For Any Other Combination
                e_cUsage = %Ucs2('U');
              EndIf;
            Else;
              //If Colon ':' Not Found It Means Usage Is Single
              //Attribute Eg.*INPUT *DELETE *UPDATE *DELETE
              If %Scan(%Ucs2('*DELETE'):l_cKeyword)> 0 or
                %Scan(%Ucs2('*UPDATE'):l_cKeyword) >0 ;
                //*DELETE AND *DELETE BOTH ARE OPEN WITH U
                e_cUsage = %Ucs2('U');
                //*OUTPUT  OPEN WITH O
              ElseIf %Scan(%Ucs2('*OUTPUT'):l_cKeyword) >0 ;
                e_cUsage = %Ucs2('O');
              Else;
                 //If None Of 'U'or 'O'
                 e_cUsage = %Ucs2('I');
              EndIf;
            EndIf;
          EndIf;
        EndIf;
      EndIf;
    Else;
      //If Usage Not Found  Then Set Usage Default
      //*Validate Workstn Keyword
      If %Scan(c_cWrkstn :e_cString) > 0;
        yValidateDevice(e_cString:c_cWrkstn :l_lok);
        If l_lok = *On;
          e_cUsage = %Ucs2('C');
        EndIf;
      EndIf;

      If %Scan(c_cPrntr :e_cString) > 0;
      //*Validate Printer Keyword
        yValidateDevice(e_cString:c_cPrntr :l_lok);
        If l_lok = *On;
          e_cUsage = %Ucs2('O');
        EndIf;
      EndIf;
    EndIf;
    //*Set Device If Workstn and Printer Not Valid
    If e_cUsage = %Ucs2(' ');
      e_cUsage = %Ucs2('I');
    EndIf;
  EndSr;


End-Proc;

//-------------------------------------------------------*
//Set File Format Eg. Extenally File Or Program Described
//-------------------------------------------------------*
Dcl-Proc ychkpgmdesc;
  Dcl-Pi ychkpgmdesc;
    e_cString ucs2(500);
    e_ckeywd  ucs2(10) Const;
    e_cFilFmt ucs2(1);
    e_nRfLen  packed(5:0);
  End-Pi;

  dcl-s l_cKeyword  Ucs2(500);
  dcl-s l_cRfLen    Ucs2(20);
  dcl-s l_nPos1     packed(5:0);
  dcl-s l_nPos2     packed(5:0);
  l_cKeyword  = e_cString;
  l_nPos1 = %Scan(%Trim(e_ckeywd) : l_cKeyword);
  If l_nPos1 > 0;
    l_cKeyword = %TrimL(%Subst(e_cString:l_nPos1+ %len(%trim
                                          (e_ckeywd))));
    If %Subst(l_cKeyword:1:1) = c_cLpa;
      l_nPos1 = %Scan(c_cLpa : l_cKeyword);
      If l_nPos1 > 0;
        l_nPos2 = %Scan(c_cRpa:l_cKeyword :l_nPos1);
        If l_nPos2 > 0;
          //Check For Device(*ext) And Scan '*'If Not Found  Then
          //If Disk(Variable) or Variable = 112  Then
          If %Subst(%TrimL(%Subst(l_cKeyword:l_nPos1+1)):1:1)<>c_cAst;
            //Program Described File Then File Format 'F'
            e_cFilFmt = %Ucs2('F');
            //Record length
            l_cRfLen = %trim(%Subst(l_cKeyword:2:l_npos2-2));
            If %check(%ucs2('0123456789'):%trim(l_cRfLen))= *zeros;
              // %PRECPL CVT_FROM_UNICODE l_cRfLen
              e_nRfLen = %int(%trim(l_aRfLen));
              //IfConstant Value Then Eg.Device(LEN)
            Else;
              If l_cRfLen <> *Blanks;
                If %Scan(c_cElem:l_cRfLen)   > *Zeros
                  Or %Scan(c_cSize:l_cRfLen) > *Zeros
                  Or %Scan(c_cLen:l_cRfLen)  > *Zeros;
                  l_nPos1 = %Scan(c_cLpa : l_cRfLen );
                  //*As of now, we are dealing single level BIF and not BIF
                  //*inside BIF like DIM(%SIZE(%scan(v1:V2))) or DIM(%size(%trim(.
                  If l_nPos1 > *Zeros;
                    //*Store Constant Value  Of For After Process
                    Ds_wConsKwd(i_nconskwd).s_cConsnam  = %Subst(l_cRfLen:
                                 l_nPos1 + 1 );
                    Ds_wConsKwd(i_nconskwd).s_cBIf = %Subst(l_cRfLen: 1 :
                                                      l_nPos1 - 1 );
                    //*Store FileName Corresponding Of Constant Value
                    Ds_wConsKwd(i_nconskwd).s_cFilname
                                         =  Ds_wFilwrt.s_cFilnam;
                    //*Store Bif Name
                    i_nconskwd += 1;
                  EndIf;
                Else;
                  //*Store Constant Value  Of For After Process
                  Ds_wConsKwd(i_nconskwd).s_cConsnam  =  l_cRfLen ;
                  //*Store FileName Corresponding Of Constant Value
                  Ds_wConsKwd(i_nconskwd).s_cFilname
                                         =  Ds_wFilwrt.s_cFilnam;
                  i_nconskwd += 1;
                EndIf;
              EndIf;
            Endif;
          Else;
            //If  Device(*ext)then it is external File
            e_cFilFmt = %Ucs2('E');
          EndIf;
        EndIf;
      EndIf;
    Else;
      e_cFilFmt = %Ucs2('E');
    EndIf;
  EndIf;
End-Proc;

//--------------------------------------------------------------------------------*
//Procedure To Validate Device Keyword  (Eg. Device Printer Workstn )
//--------------------------------------------------------------------------------*

Dcl-Proc yValidateDevice;
  Dcl-Pi yValidateDevice;
    e_cString   Ucs2(500);
    e_cDivKwd   Ucs2(10) Const;
    e_lOk       ind;
  End-Pi;
  dcl-s l_cwrkStr    ucs2(500);
  dcl-s l_nPos1     packed(5:0);
  dcl-s l_nPos2     packed(5:0);
  dcl-s l_nPos3     packed(5:0);
  e_lOk   = *Off;
  l_nPos1 = %Scan(%Trim(e_cDivKwd):e_cString);
  l_nPos3 = l_nPos1 + %len(%Trim(e_cDivKwd));
  //*If Position of First Left Parenthesis '( ' Is Greater Than Pos Of
  // Device Eg. Disk Workstn Printer Or After Device FirstCharIs ; Smc
  //*The First Keyword Must Be Device Keyword
  If l_nPos1 > 0;
    l_nPos2 = %Scan(c_cLpa :e_cString);
    If l_nPos2 > 0 And l_nPos1 < l_nPos2 ;
      e_lOk  = *On;
    Else;
      l_cwrkStr = %Trim(%Subst(e_cString : l_nPos3 )) ;
      If %Subst(l_cwrkstr:1:1) <> c_cRPa  and
         %Subst(l_cwrkstr:1:1) <> c_cCln;
          e_lOk  = *On;
      EndIf;
    EndIf;
  EndIf;
End-Proc;

//*=====================================================
//Procedure To process F Spec
//*=====================================================
Dcl-Proc yPrcFixFile Export;
  Dcl-Pi yPrcFixFile;
    e_IRepId packed(9:0);
    e_ISrcId packed(9:0);
    e_IPrcId packed(9:0);
    e_WSrc   likeds(DS_WSRC);
    e_wRRN   likeds(DS_WRRN);
  End-Pi;

  dcl-s l_cString   ucs2(500);
  dcl-s l_cwrkstr   ucs2(500);
  dcl-s l_nkwdsize  packed(5:0);
  dcl-s l_nIdx      packed(2:0) inz(1);
  dcl-s l_nPos      packed(2:0) inz(1);
  dcl-s l_nPos1     packed(5:0);
  dcl-s l_nPos2     packed(5:0);
  dcl-s l_nPos3     packed(5:0);
  dcl-s l_cArrval   ucs2(10) Dim(99);
  dcl-ds lDs_wLikKwd LikeDs(Ds_wLikKwd);
  Clear lDs_wLikKwd;
  clear Ds_wFilWrt;
  Ds_wFilwrt.s_iRepId  = e_IRepId; //Repository ID
  Ds_wFilwrt.s_iSrcid  = e_ISrcId; //Member ID
  Ds_wFilwrt.s_iPrcid  = e_IPrcId; //Procedure ID
  Ds_wFilwrt.s_cFilnam = e_WSrc.s_cFileName; //Program File Name
  Ds_wFilwrt.s_cFilFmt = %Subst(e_WSrc.s_CSRCCODE
                           :22:1);  // File Format(E/F)
  Ds_wFilwrt.s_cUsage  = %Subst(e_WSrc.s_CSRCCODE
                           :17:1); // Usage Type
  Ds_wFilwrt.s_cDevice = e_Wsrc.s_cDevice;//Device Name

  l_cwrkstr = %Subst(e_WSrc.s_CSRCCODE:25:5);

  //Get Record Length If File is Programmed Described
  If Ds_wFilwrt.s_cFilFmt = %UCS2('F'); //Do if File Format is F
    If %check(%ucs2('0123456789'):%trim(l_cwrkstr)) = *Zeros;
      //%PRECPL CVT_FROM_UNICODE l_cwrkstr
      Ds_wFilwrt.s_nRflen = %int(%trim(l_awrkstr));
    Endif;
  Endif;


  l_cString = %Subst(e_WSrc.s_cLngKwd:44);

//--------------------------------------------------------*
// Process LIKEFILE Keyword -----------------------------*
//--------------------------------------------------------*
  If %Scan(c_cLikfil:l_cString) > *Zeros;
    yPrcKwds(l_cString:c_cLikfil:l_cArrval:l_nkwdsize);
    if l_nkwdsize > 1;
      lDs_wLikKwd.s_cOrgfil =  Ds_wFilwrt.s_cFilnam; //Internal File
      lDs_wLikKwd.s_cLikfil =  l_cArrval(1);         //External File

      //Procedure to get the data of Likefile from PGI and WFD and
      //Write Corresponding to the Internal Described Dependent File
      YPrcLikeFile(e_IRepId:e_ISrcId:e_wRRN:lDs_wLikKwd);
      Return;
    EndIf;
  EndIf;
//--------------------------------------------------------*
// Get the External File Name if Found
//--------------------------------------------------------*
  YGetPgmInf(Ds_wFilwrt.s_cfilnam:Ds_wFilwrt.s_cextfilnam);

//--------------------------------------------------------*
//  Process ALIAS keyword --------------------------------*
//--------------------------------------------------------*

  Ds_wFilwrt.s_cAlias=%Ucs2('N');
  l_npos = %Scan(%Ucs2('ALIAS '):l_cString);
  Dow l_nPos > *Zeros;
    l_cwrkstr = %trim(%subst(l_cString:l_npos + 6));
    If %Subst(l_cwrkstr:1:1) <> c_cRPa and
      %Subst(l_cwrkstr:1:1) <> c_cCln;
      Ds_wFilwrt.s_cAlias=%Ucs2('Y');
      leave;
    Endif;
    l_npos = %Scan(%Ucs2('ALIAS '):l_cString:l_nPos+1);
  EndDo;

//--------------------------------------------------------*
//  Process PREFIX keyword --------------------------------*
//--------------------------------------------------------*
  If %Scan(c_cPrefix:l_cString) > *Zeros;
    yPrcKwds(l_cString:c_cPrefix:l_cArrval:l_nkwdsize);
    If l_nkwdsize > 1 ;
      Ds_wFilwrt.s_cPrefix = l_cArrval(1);
      //If Prefix Position Non Blank then Store
      If l_cArrval(2) <> *Blanks;
        Ds_wFilwrt.s_nPfxpos = %dec(%char(l_cArrval(2)):3:0);
      Endif;
      l_nPos1 = %scan(c_cQut: Ds_wFilwrt.s_cPrefix ) ;
      If l_nPos1 > *Zeros;
        l_nPos2 = %Scan(c_cQut: Ds_wFilwrt.s_cPrefix : l_nPos1+1);
        If l_nPos2 > *Zeros;
           l_nPos3 = l_nPos2 - l_nPos1 ;
          If l_nPos3 > 1 ;
            Ds_wFilwrt.s_cPrefix = %Subst(Ds_wFilwrt.s_cPrefix
                                        : 2 : l_nPos2 - 2 ) ;
          Else;
            Ds_wFilwrt.s_cPrefix = *Blanks;
          EndIf;
        EndIf;
      EndIf;
    EndIf;
  EndIf;

//--------------------------------------------------------------
//    Process DATFMT keyword
//--------------------------------------------------------------
  If %Scan(%trim(c_cDatfmt):l_cString) > *zeros;
    yPrcKwds(l_cString:c_cDatfmt:l_cArrval:l_nkwdsize);
    If l_nkwdsize > 1;
      Ds_wFilwrt.s_cDatfmt = l_cArrval(1);
    EndIf;
  EndIf;

//--------------------------------------------------------------
//   Process Rename keyword
//--------------------------------------------------------------
  If %Scan(c_cRename:l_cString) > *zeros;
    yPrcKwds(l_cString:c_cRename:l_cArrval:l_nkwdsize);
    If l_nkwdsize > 1;
      l_nPos = 1;
      For l_nIdx=1 to l_nkwdsize-1;
        Ds_wFilwrt.s_cOrgRf(l_nPos) = l_cArrval(l_nIdx);
        l_nIdx += 1;
        Ds_wFilwrt.s_cRnmRf(l_nPos) = l_cArrval(l_nIdx);
        l_nPos += 1;
      EndFor;
    EndIf;
  EndIf;

//-------------------------------------------------------------
//   Process Ignore keyword
//--------------------------------------------------------------
  If %Scan(c_cIgnore:l_cString) > *zeros;
    yPrcKwds(l_cString:c_cIgnore:l_cArrval:l_nkwdsize);
    //Check if w_cArrval is not Blank;
    For l_nIdx=1 to l_nkwdsize-1;
      Ds_wFilwrt.s_cIgnRf(l_nIdx) = l_cArrval(l_nIdx);
    Endfor;
  EndIf;

//--------------------------------------------------------------
//   Process Include keyword
//-------------------------------------------------------------
  If %Scan(c_cInclude:l_cString) > *zeros;
    yPrcKwds(l_cString:c_cInclude:l_cArrval:l_nkwdsize);
    for l_nIdx=1 to l_nkwdsize-1;
      Ds_wFilwrt.s_cincrf(l_nIdx) = l_cArrval(l_nIdx);
    Endfor;
  EndIf;

//--------------------------------------------------------------
//   Process Sfile keyword
//--------------------------------------------------------------
  If %Scan(c_cSfile:l_cString) > *zeros;
    yPrcKwds(l_cString:c_cSfile:l_cArrval:l_nkwdsize);
    l_nPos = 1;
    for l_nIdx=1 to l_nkwdsize-1 by 2;
      Ds_wFilwrt.s_cSfile(l_nPos) = l_cArrval(l_nIdx);
      l_nPos += 1;
    Endfor;
  EndIf;

  if e_wRRn.s_nSrcSeq = *zeros;
    ds_wfilwrt.s_wbgn = e_wRRn;
    ds_wfilwrt.s_wend = e_wRRn;
  else;
    ds_wfilwrt.s_wend = e_wrrn;
  endif;

  //Procedure to write information
  YWrtFileFld(Ds_wFilwrt:e_wRRn:e_wSrc);

  *inlr = *on;

End-Proc;

//--------------------------------------------------------------
//   Procedure:yPrcKwds (Process Keyword in given String)
//--------------------------------------------------------------
Dcl-Proc yPrcKwds;
  Dcl-Pi yPrcKwds;
    el_csrcstr  ucs2(500);
    el_ckwdnam  ucs2(10) const;
    el_ckwdval  ucs2(10) dim(99);
    el_nkwdsize  packed(5:0);
  End-Pi;

  dcl-s l_nPos1     packed(5:0);
  dcl-s l_nPos2     packed(5:0);
  dcl-s l_nIdx      packed(1:0) Inz(1);
  dcl-s l_ckwdval   ucs2(100);
  dcl-s i_narrval   packed(1:0) Inz(1);
  clear el_ckwdval ;
  el_nkwdsize = 1 ;
  dow 1 = 1;
    l_nPos1 = %scan(%trim(el_ckwdnam):el_csrcstr);
    l_npos2 = *zeros;
    dow l_npos1 > *zeros;

    //Make sure this is real keyword with open/close brackets and not part of string
      l_ckwdval = %trim(%subst(el_csrcstr:
                    l_npos1 + %len(%trim(el_ckwdnam))));
      If %Subst(l_ckwdval:1:1) = c_cLPa;
        l_npos2 = %scan(c_crpa:el_csrcstr:l_npos1);
        If l_npos2 > *zeros;
          el_csrcstr = %replace('':el_csrcstr:
                              l_npos1:(l_npos2+1)-l_npos1);
        Else;
          leave;
        EndIf;
        l_npos2 = 1;
        leave;
      EndIf;
      l_nPos1 = %Scan(%trim(el_ckwdnam):el_csrcstr:l_npos1 + 1);
    EndDo;

    If l_nPos2 = *Zeros;
      return;
    EndIf;

    l_ckwdval = %trim(%subst(l_ckwdval:2));
    l_npos1 = %scan(c_crpa:l_ckwdval);
    If l_npos1 > *zeros;
      l_ckwdval = %subst(l_ckwdval:1:l_npos1-1);
    EndIf;

    //Get the Position of Colon(:)
    l_npos1 = %scan(c_ccln:l_ckwdval);
    Dow l_npos1 > *Zeros or l_ckwdval <> *Blanks;
      If l_npos1 = *zeros;
        l_npos1 = %len(%trim(l_ckwdval)) + 1;
      EndIf;
      el_ckwdval(i_narrval) = %subst(l_ckwdval:1:l_npos1-1);
      i_narrval = i_narrval + 1;
      l_ckwdval = %trim(%subst(l_ckwdval:l_npos1+1));
      l_npos1 = %scan(c_ccln:l_ckwdval);
      el_nkwdsize = i_narrval;
    EndDo;
  EndDo;
End-Proc;
//*==================================================================
//  Cursor to fetch the data from DSPPGMREF
//*==================================================================
Dcl-Proc YGetPgmInf;
  Dcl-Pi *n;
    pl_cFilnam  ucs2(10);
    pl_cextfilnam ucs2(10);
  End-Pi;
    Exec sql
       select whfnam into :pl_cextfilnam from aarireff1 where whsnam = :pl_cfilnam;
End-Proc;
//*==================================================================
    //Write the data in Work Field File
//*==================================================================
Dcl-Proc YWrtFileFld;
  Dcl-Pi *n;
    pl_filedtl likeds(Ds_wFilwrt);
    pl_fileRRn likeds(Ds_WRRN);
    pl_fileSrc likeds(Ds_wSRC);
  End-Pi;

  Dcl-s l_cvarname  ucs2(50) ;
  Dcl-s l_nObjId    packed(9:0);
  Dcl-s l_iRepId    packed(9:0);
  Dcl-s l_nPos1     packed(5:0);
  Dcl-s l_nIdx      packed(2:0) inz(1);
  Dcl-s l_cFileFld  ucs2(50);
  Dcl-s l_cfilenam  ucs2(50);
  Dcl-s l_cerrdt    ucs2(100);

  l_nPos1    = pl_filedtl.s_nPfxPos;    //Store the Prefix Position
  l_cfilenam = pl_filedtl.s_cextfilnam; //File Name
  l_irepid   = pl_filedtl.s_IRepId;     //Repository ID

  If l_cfilenam <> *Blanks;
    l_nObjId = yGetobjId(l_irepid:l_cfilenam); //Get File Object ID
  Endif;

  //  Subroutine to Write Program Info into PGI
  Exsr z_Wrtpgidtl;
  If not %open(AARIWFDF1);
    Open(e) AARIWFDF1;
  Endif;

  If not %open(AARIFFDL2);
    Open(e) AARIFFDL2;
  Endif;
  If l_nObjId > 0;
    Setll(e) (l_irepid:l_nObjId) aariffdl2;
    Reade(e) (l_irepid:l_nObjId) aariffdl2;

    Clear AARIWFDFF;
    Dow not %eof(aariffdl2);
      If ffd_jflde = *blanks and ffd_calis = *blanks;
        Reade(e) (l_irepid:l_nObjId) AARIFFDl2;
        Iter;
      Endif;

      //Set the File Field Name with or without Prefix as decalred
      If pl_filedtl.s_nPfxPos > 0 and pl_filedtl.s_cAlias = %Ucs2('N');
        l_cFileFld = %Trim(pl_filedtl.s_cPrefix) +
                     %Trim(%Subst(ffd_jflde:l_nPos1+1));

      ElseIf pl_filedtl.s_nPfxPos > 0 and pl_filedtl.s_cAlias = %Ucs2('Y')
                                     and FFD_CALIS <> *blanks;
        l_cFileFld = %Trim(pl_filedtl.s_cPrefix) +
                     %Trim(%Subst(FFD_CALIS:l_nPos1+1));

      Elseif pl_filedtl.s_cAlias = %Ucs2('N');
        l_cFileFld = %Trim(pl_filedtl.s_cPrefix) + %Trim(FFD_JFLDE);

      Elseif pl_filedtl.s_cAlias = %Ucs2('Y');
        l_cFileFld = %Trim(pl_filedtl.s_cPrefix) + %Trim(FFD_CALIS);
      EndIf;

      wfd_irepid = l_irepid ;                     //Repository Id
      wfd_imbrid = pl_filedtl.s_ISrcId;           //Source Member Id
      wfd_iprcid = pl_filedtl.s_IPrcId;           //Procedure Id
      wfd_nseqs  = pl_filedtl.s_wbgn.s_nSrcSeq;   //Start Source Seq
      wfd_nseqe  = pl_filedtl.s_wend.s_nSrcSeq;   //End Source Seq
      wfd_nrrns  = pl_filedtl.s_wbgn.s_nOrgSrcRRN;//Start Source Rrn
      wfd_nrrne  = pl_filedtl.s_wend.s_nOrgSrcRRN;//End Source Rrn
      wfd_jfilnm = pl_filedtl.s_cextfilnam;       //External File
      wfd_jfldnm = FFD_JFLDI;                     //External Field
      wfd_nfrbuf = FFD_NIBO;                      //From Buffer Pos
      wfd_ntobuf = FFD_NIBO + FFD_NFLDB - 1;      //To Buffer Pos
      wfd_cdfmt  = FFD_CFMT;                      //Date Fmt
      wfd_ndecp  = FFD_NFLDP;                     //Dec Pos
      wfd_cdtyp  = FFD_CFLDT;                     //Data Type
      wfd_cvtyp  = %Ucs2('EF');                   //Variable Type
      wfd_ccset  = *Blanks;                       //Char Set
      wfd_cspca  = *Blanks;                       //Special Attributes
      wfd_cseca  = %Ucs2('NN');                   //Secondary Attr
      wfd_ncrrns = pl_filedtl.s_wbgn.s_nCpybkRRN; //Cpybk Start RRN
      wfd_ncrrne = pl_filedtl.s_wend.s_nCpybkRRN; //Cpybk End RRN
      wfd_ncseqs = pl_filedtl.s_wbgn.s_nCpybkSeq; //Cpybk Start Seq
      wfd_ncseqe = pl_filedtl.s_wend.s_nCpybkSeq; //Cpybk End Seq
      wfd_ndim   = 0;                             //Array Dim
      wfd_iftxid = 0;                             //Field description ID
      wfd_ividl  = 0;                             //Long qualified name ID
      wfd_idsid  = 0;                             //DS Id

      If FFD_CVARL = %ucs2('Y');
        wfd_Cvltyp = %ucs2('V2');                 //Varying Type
      Endif;

      If ffd_nfldg > 0;
        wfd_nlen = ffd_nfldg; //Field Length
      ElseIf ffd_nfldd > 0;
        wfd_nlen = ffd_nfldd; //Number of Digits
      Else;
        wfd_nlen = ffd_nfldb; //Field Length in Bytes
      EndIf;

      wfd_ikwid  = nGetKeywordId(l_irepid:pl_fileSrc.s_cLngKwd); //Keyword Id

      l_cvarname = wfd_jfldnm;
      wfd_ivarid = yGetVarID(l_irepid:l_cFileFld); //Variable Id

      wfd_icpyid = *zeros;
        If pl_fileSrc.s_ccpymbrf <> *Blanks and
           pl_fileSrc.s_ccpysrcf <> *Blanks and
           pl_fileSrc.s_ccpyslib <> *Blanks;
                                                //Copybook Member Id
          wfd_icpyid = YGetSrcId(l_irepid:pl_fileSrc.s_cCpyMbrf:
                       pl_fileSrc.s_cCpySrcf:pl_fileSrc.s_cCPYSLIB);

        Endif;
      Setll (wfd_irepid:wfd_imbrid:wfd_iprcid:wfd_ivarid) aariwfdl1;
      If not %equal;  //Write if there is no Existing Variable Id;
        Write(e) AARIWFDFF;
      Endif;
      Reade(e) (l_irepid:l_nObjId) aariffdl2;
    Enddo;
    Close(e) AARIFFDL2;
    Close(e) AARIWFDF1;
  Else;
    l_cerrdt = 'Declared File ' + %Trim(pgi_jfobji) + ' not found';
    //yErrorLog();
  Endif;

//*==================================================================
//Subroutine: Write Program info into PGI
//*==================================================================

  Begsr z_Wrtpgidtl;
    If not %open(AARIPGIF1);
      Open(e) AARIPGIF1;
    Endif;
    Clear AARIPGIFF;
    pgi_irepid = l_irepid ;            //Repository ID
    pgi_isrcid = pl_filedtl.s_ISrcId;  //Source ID
    pgi_jfobji = pl_filedtl.s_cFilnam; //Internal File Name
    pgi_jfobje = l_cfilenam;           //External File Name
    pgi_ifobid = l_nObjId;             //Object ID
    pgi_cprefx = pl_filedtl.s_cPrefix; //Prefix Name
    pgi_nprefx = pl_filedtl.s_nPfxPos; //Prefix Position

    If %trim(pgi_jfobji) <> %trim(pgi_jfobje);
      pgi_cexinf = %ucs2('Y');         //if Internal and Extrernal...
    Endif;                             //..file Name is not Matched

    //if Renamed Rcdfmt is not Found
    If pl_filedtl.s_cRnmRf(1) = *Blanks;
      //Find original record format
      Chain(e) (l_irepid:l_nObjId) AARIFFIF1; //Get Original RCDFMT
      If %Found(AARIFFIF1);
        pgi_jorgrf = ffi_jrfnam;  //Original Record Format Name
        Chain(e) (PGI_IREPID : PGI_IFOBID : PGI_JFOBJI :PGI_ISRCID:
                  PGI_JORGRF) AARIPGIF1;
        If Not %Found(AARIPGIF1);
          //Write record
          Write(e) AARIPGIFF; //Write info into PGI File
        Endif;
      Endif;
    Else;  //Read the Array and write the RCDFMT into File
      Dow pl_filedtl.s_cOrgRf(l_nIdx) <> *blanks or
          pl_filedtl.s_cRnmRf(l_nIdx) <> *blanks ;
        pgi_jorgrf = pl_filedtl.s_cOrgRf(l_nIdx); //Original RCDFMT
        pgi_jrnmrf = pl_filedtl.s_cRnmRf(l_nIdx); //Renamed RCDFMT
        l_nIdx += 1;
        Chain(e) (PGI_IREPID : PGI_IFOBID : PGI_JFOBJI: PGI_ISRCID:
                  pgi_jorgrf:pgi_jrnmrf)AARIPGIF1;
        If Not %Found(AARIPGIF1); //Write if there is no Existing...
                                  //...Record Format Found
          Write(e) AARIPGIFF;
        Endif;
      Enddo;
    Endif;

    if pl_filedtl.s_cSfile(1) <> *Blanks;
      l_nIdx = 1;
      Dow pl_filedtl.s_cSfile(l_nIdx) <> *blanks ;
        pgi_jorgrf = pl_filedtl.s_cSfile(l_nIdx); //Sfile RCDFMT
        l_nIdx += 1;
        Chain(e) (PGI_IREPID : PGI_IFOBID : PGI_JFOBJI: PGI_ISRCID:
                 PGI_JORGRF)AARIPGIF1;
        If Not %Found(AARIPGIF1); //Write if there is no Existing...
                                 //...Record Format Found
          PGI_JRNMRF = *Blanks;
          Write(e) AARIPGIFF;
        Endif;
      Enddo;
    Endif;
    if pl_filedtl.s_cIgnRf(1) <> *Blanks;
      l_nIdx = 1;
      Dow pl_filedtl.s_cIgnRf(l_nIdx) <> *blanks ;
        PGI_JORGRF = pl_filedtl.s_cIgnRf(l_nIdx); //Ignored RCDFMT
        l_nIdx += 1;
        Chain(e) (PGI_IREPID : PGI_IFOBID : PGI_JFOBJI: PGI_ISRCID:
                 PGI_JORGRF)AARIPGIF1;
        If Not %Found(AARIPGIF1); //Write if there is no Existing...
                                 //...Record Format Found
          PGI_JRNMRF = *Blanks;
          Write(e) AARIPGIFF;
        Endif;
      Enddo;
    Endif;
    if pl_filedtl.s_cIncRf(1) <> *Blanks;
      l_nIdx = 1;
      Dow pl_filedtl.s_cIncRf(l_nIdx) <> *blanks ;
        PGI_JORGRF = pl_filedtl.s_cIncRf(l_nIdx); //Include RCDFMT
        l_nIdx += 1;
        Chain(e) (PGI_IREPID : PGI_IFOBID : PGI_JFOBJI: PGI_ISRCID:
                 PGI_JORGRF)AARIPGIF1;
        If Not %Found(AARIPGIF1); //Write if there is no Existing...
                                 //...Record Format Found
          PGI_JRNMRF = *Blanks;
          Write(e) AARIPGIFF;
        Endif;
      Enddo;
    Endif;
    Close(e) AARIPGIF1;
  Endsr;

 End-Proc;

//*==================================================================
    //Get the source ID
//*==================================================================

Dcl-Proc YGetSrcId;
  Dcl-Pi *n packed(9:0);
    pl_irepid packed(9:0);
    pl_csmbr ucs2(10);
    pl_csrcf ucs2(10);
    pl_cslib ucs2(10);
  End-Pi;

 Dcl-s l_nmbrid packed(9:0);
 Open(e) aarismbl2;
 Open(e) aarilbsl1;
 Chain(e) (pl_irepid:pl_csrcf:pl_cslib) aarilbsl1;
 If %found(aarilbsl1);
   Chain(e) (pl_irepid:pl_csmbr:lbs_isrcid) aarismbl2;
   If %found(aarismbl2);
     l_nmbrid = smb_imbrid;
   Endif;
 Endif;
 Return l_nmbrid;

 End-Proc;

//*==================================================================
    //Get the Variable ID
//*==================================================================
 Dcl-Proc yGetVarID;
   Dcl-Pi *n packed(9:0);
     e_IRepId   packed(9:0);
     e_JVarName ucs2(50) ;
   End-Pi;

   Dcl-s l_IRtnVarId  packed(9:0) Inz;
     If %trim(e_JVarName) <> *blanks;
       If not %Open(AARIVRIF1);
         Open(E) AARIVRIF1;
       EndIf;
       If not %Open(AARIVRIL1);
         Open(E) AARIVRIL1;
       EndIf;

       If Not %Error;
         If %Subst(e_JVarName:1:1) = %Ucs2('''');
           e_JVarName = %Subst(e_JVarName:2);
         EndIf;
         If e_JVarName <> *Blanks
           And %Subst(e_JVarName:%Len(%Trim(e_JVarName)):1) =
                                                         %Ucs2('''');
           e_JVarName = %Subst(e_JVarName:1:%Len(%Trim(e_JVarName))-1);
         EndIf;
         If e_JVarName <> *Blanks;
           Chain(e) (e_IRepId :e_JVarName) AARIVRIL1;
           If %Found(AARIVRIL1);
             l_IRtnVarId = VRI_IVARID;
           Else;
             VRI_IVARID = *HIVAL;
             setgt(e) (e_IRepId :VRI_IVARID) AARIVRIF1;
             readpe(e) e_IRepId AARIVRIF1;
             If Not %EOF(AARIVRIF1) And Not %error;
               l_IRtnVarId = VRI_IVARID +1;
             Else;
               l_IRtnVarId = 1;
             EndIf;
             VRI_IREPID = e_IRepId;
             VRI_IVARID = l_iRtnVarId;
             VRI_CVARNM = e_JVarName;
             Write(e) AARIVRIFF;
           EndIf;
         EndIf;
       Endif;
       Close AARIVRIF1;
       Close AARIVRIL1;
     Else;
       l_IRtnVarId = *zeros;
     Endif;
     return l_IRtnVarId;
 End-Proc;

 // ===============================================================         ==*
 //Procedure:Return Keyword Id
 // ===============================================================         ==*

 Dcl-proc nGetKeywordId;
   Dcl-pi nGetKeywordId   packed(9:0);
     e_IRepId    packed(9:0);
     e_cKeyWord   ucs2(500);
   end-pi;
   Dcl-s l_iKwdId  packed(9:0);
     l_iKwdId = 0;
     If e_cKeyword = *Blanks;
       Return l_iKwdId;
     EndIf;
     //Get Keyword Id;
     YAARI022R1(e_IRepId :e_cKeyword :l_iKwdId);
     //Return keyword Id
     Return l_IKwdId;
 End-proc;

//*==================================================================
    //Get the Object ID
//*==================================================================

 Dcl-proc yGetobjId;
   Dcl-pi yGetobjId  packed(9:0);
     e_IRepId    packed(9:0);
     e_cObjName  Ucs2(10);
   End-pi;

   Dcl-s w_cObjTyp ucs2(10) inz('*FILE');

   If not %Open(aariobjl3);
     Open(e) aariobjl3;   //Open File
   Endif;

   Chain(e) (e_IRepId:e_cObjName:w_cObjTyp) AARIOBJL3; //Get Object ID
   If %Found(AARIOBJL3);
     return OBJ_IOBJID;  //Return Object ID if Found
   Else;
     return 0;           //Return 0 if object ID is Not Found
   Endif;
 End-proc;

//*==================================================================
//Procedure to Process Likefile
//*==================================================================

 Dcl-Proc YPrcLikeFile;
   Dcl-Pi *N;
     e_IRepId Packed(9:0);
     e_ISrcId Packed(9:0);
     e_wRRN   LikeDs(Ds_wRRN);
     e_wLikds Likeds(Ds_wLikKwd);
   End-Pi;
   Dcl-s  l_cOrgFile    Ucs2(10);
   Dcl-s  l_cLikFile    Ucs2(10);
   Dcl-s  l_nObjId      Packed(9:0);
   Dcl-s  l_cVa        Ucs2(10) inz('Printr');
   l_cOrgFile = e_wLikds.s_cOrgfil;  // Internal File Name
   l_cLikFile = e_wLikds.s_cLikfil;  // External File Name

   If l_cLikFile <> *Blanks;
     l_nObjId = yGetobjId(e_IRepId:l_cLikFile); //Get File Object ID
   Endif;

   if l_nObjId > 0; //If Object id is Found
     If not %open(AARIPGIF1);          //Open File
       Open(e) AARIPGIF1;
     Endif;

     //If Like File is found then fetch the relative record and write
     //with correponding to the internal file name.
     Setll(e) (e_IRepId:l_nObjId:l_cOrgFile:e_ISrcId) AARIPGIF1;
     Reade(e) (e_IRepId:l_nObjId:l_cOrgFile:e_ISrcId) AARIPGIF1;
     Dow Not %Eof(AARIPGIF1);
       Clear AARIPGIFF;
       pgi_irepid = pgi_irepid;   //Repository ID
       pgi_isrcid = pgi_isrcid;   //Source ID
       pgi_jfobji = l_cOrgFile;   //Internal File Name
       pgi_jfobje = pgi_jfobje;   //External File Name
       pgi_ifobid = pgi_ifobid;   //Object ID
       pgi_cprefx = pgi_cprefx;   //Prefix Name
       pgi_nprefx = pgi_nprefx;   //Prefix Position

       Reade(e)(e_IRepId:l_nObjId:l_cOrgFile:e_ISrcId) AARIPGIF1;
     EndDo;
   Endif;
 End-Proc; 
