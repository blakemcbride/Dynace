
CREATE TABLE dynace_mutex (
	tag		VARCHAR(255) NOT NULL,
	data		VARCHAR(255),
	lastupdate	DATETIME NOT NULL,
	PRIMARY KEY ( tag )
);

