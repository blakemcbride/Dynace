
CREATE TABLE Audit_Data (
       TableName		VARCHAR(35) NOT NULL,
       FieldName		VARCHAR(35) NOT NULL,
       KeyData			VARCHAR(128) NOT NULL,
       ChangeTime		DATETIME NOT NULL,
       ChangeType		CHAR(1) NOT NULL,
       UserID			INTEGER NOT NULL,
       FieldValue		VARCHAR(255),
       PRIMARY KEY ( TableName, FieldName, KeyData, ChangeTime )
);


