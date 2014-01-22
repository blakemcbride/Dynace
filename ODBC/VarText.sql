
CREATE TABLE VariableText (
    VarText                             varchar             (255) NULL,
    VarTextCount                        integer              NOT NULL,
    VarTextID                           integer              NOT NULL,
    PRIMARY KEY (VarTextID, VarTextCount)
);

CREATE UNIQUE INDEX PK_VariableText ON VariableText (
    VarTextID, VarTextCount
);
