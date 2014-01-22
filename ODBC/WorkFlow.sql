
CREATE TABLE Tasks (
	TaskID			INTEGER PRIMARY KEY,
	TaskName		VARCHAR(30) UNIQUE,
	TaskDesc		VARCHAR(60),
	TaskType		CHAR(1),
	TaskClass               VARCHAR(50),
	TrackTask		CHAR(1),
);

{    TaskType =
		S = Scheme Dialog
		W = Scheme Window
		F = Form
		G = GUI Window
		D = C Dialog
}

CREATE TABLE TaskGroup (
	TGID			INTEGER PRIMARY KEY,
	TGDesc			VARCHAR(60)
);

CREATE TABLE TaskGroupDetail (
	TGID			INTEGER,
	TGDOrder		SMALLINT,
	TaskID			INTEGER,
	PRIMARY KEY ( TGID, TGDOrder )
);






CREATE TABLE TaskListDetail (
	TLDID			INTEGER,
	TLDOrder		SMALLINT,
	TLDSubOrder		SMALLINT,
	TaskID			INTEGER,
	State			CHAR(1),   {  Active, Hidden, Disabled  }

	TSID			SMALLINT,
	DateFirstEntered	DATETIME,
	DateLastModified	DATETIME,
	DateCompleted		DATETIME,

	PRIMARY KEY ( TLDID, TLDOrder, TLDSubOrder ),
	UNIQUE ( TLDID, TaskID )
);

