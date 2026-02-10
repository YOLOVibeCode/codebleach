namespace CodeBleach.Processors.MainframeUtility;

/// <summary>
/// Types of mainframe utility control cards detected via content heuristics.
/// </summary>
public enum CardType
{
    /// <summary>DB2 BIND/RUN cards: DSN SYSTEM(...) RUN PROGRAM(...) PLAN(...) END</summary>
    Db2Bind,

    /// <summary>IDCAMS utility: DELETE, DEFINE CLUSTER, REPRO, LISTCAT</summary>
    Idcams,

    /// <summary>DFSORT/SYNCSORT: SORT FIELDS=, MERGE, INCLUDE COND=</summary>
    Sort,

    /// <summary>FTP control scripts: hostname, credentials, cd/get/put commands</summary>
    Ftp,

    /// <summary>SMTP email cards: HELO, MAIL FROM:, RCPT TO:, DATA</summary>
    Smtp,

    /// <summary>IMS MFS (Message Format Service) maps: MSG, MFLD, DFLD, FMT</summary>
    Mfs,

    /// <summary>IMS/DL1 control: CONTROL CNTL, OPTION PRINT</summary>
    DlIControl,

    /// <summary>Catch-all for short unrecognized parameter/data cards</summary>
    Parameter
}
