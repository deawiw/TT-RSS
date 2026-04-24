CREATE TABLE articles (
    news_id         BIGINT PRIMARY KEY,
    source          TEXT NOT NULL,
    title           TEXT NOT NULL,
    content_text    TEXT,
    published_at    TIMESTAMPTZ NOT NULL,
    url             TEXT NOT NULL,
    topic_tags      TEXT,
    extracted_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT chk_articles_url_not_blank
        CHECK (length(trim(url)) > 0),
    CONSTRAINT chk_articles_source_not_blank
        CHECK (length(trim(source)) > 0),
    CONSTRAINT chk_articles_title_not_blank
        CHECK (length(trim(title)) > 0)
);

CREATE UNIQUE INDEX ux_articles_url ON articles (url);
CREATE INDEX ix_articles_published_at ON articles (published_at);
CREATE INDEX ix_articles_source ON articles (source);

CREATE TABLE etl_runs (
    run_id               BIGSERIAL PRIMARY KEY,
    started_at           TIMESTAMPTZ NOT NULL,
    finished_at          TIMESTAMPTZ,
    status               VARCHAR(20) NOT NULL,
    raw_count            INTEGER NOT NULL DEFAULT 0,
    normalized_count     INTEGER NOT NULL DEFAULT 0,
    inserted_count       INTEGER NOT NULL DEFAULT 0,
    duplicate_count      INTEGER NOT NULL DEFAULT 0,
    invalid_count        INTEGER NOT NULL DEFAULT 0,
    message              TEXT,

    CONSTRAINT chk_etl_runs_status
        CHECK (status IN ('started', 'success', 'failed'))
);

CREATE INDEX ix_etl_runs_started_at ON etl_runs (started_at);
CREATE INDEX ix_etl_runs_status ON etl_runs (status);

CREATE TABLE fraud_articles (
    news_id              BIGINT PRIMARY KEY,
    selected_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    theme_category       VARCHAR(100),
    selection_method     VARCHAR(50),

    CONSTRAINT fk_fraud_articles_news
        FOREIGN KEY (news_id)
        REFERENCES articles (news_id)
        ON DELETE CASCADE
);

CREATE INDEX ix_fraud_articles_selected_at ON fraud_articles (selected_at);
CREATE INDEX ix_fraud_articles_theme_category ON fraud_articles (theme_category);
CREATE INDEX ix_fraud_articles_selection_method ON fraud_articles (selection_method);