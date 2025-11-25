import os
from glob import glob
import sqlite3
import pandas as pd
def parse_data(force: bool = False) -> tuple[sqlite3.Connection, sqlite3.Cursor]:
    if os.path.exists("./data.db") and (not force):
        sql = sqlite3.connect("data.db")
        cursor = sql.cursor()
        return (sql, cursor)
    else:
        sql = sqlite3.connect("data.db")
        cursor = sql.cursor()
        terc = pd.read_csv('data/TERC_Urzedowy_2025-10-26.csv', sep=';', dtype=str)
        terc_woj = terc.loc[pd.isna(terc['POW'])&pd.isna(terc['GMI']), ['WOJ', 'NAZWA']] \
        .assign(NAZWA=lambda df: df['NAZWA'].str.lower())
        terc_pow = terc.loc[(~pd.isna(terc['POW']))&pd.isna(terc['GMI']), ['WOJ', 'POW', 'NAZWA']] \
        .assign(POW=lambda df: df['WOJ']+df['POW'])
        terc_gmi = terc.loc[(~pd.isna(terc['POW']))&(~pd.isna(terc['GMI'])), ['WOJ', 'POW', 'GMI', 'RODZ', 'NAZWA']] \
        .assign(GMI=lambda df: df['WOJ']+df['POW']+df['GMI']+df['RODZ'], POW=lambda df: df['WOJ']+df['POW'])
        terc_rodz = terc.loc[(~pd.isna(terc['POW']))&(~pd.isna(terc['GMI'])), ['RODZ', 'NAZWA_DOD']] \
        .drop_duplicates('RODZ').sort_values('RODZ')
        terc_woj.to_sql('wojewodztwa', sql, if_exists='replace', index=False)
        sql.execute("CREATE UNIQUE INDEX idx_woj ON wojewodztwa (WOJ);")
        terc_pow.to_sql('powiaty', sql, if_exists='replace', index=False)
        sql.execute("CREATE UNIQUE INDEX idx_pow ON powiaty (POW);")
        terc_gmi.to_sql('gminy', sql, if_exists='replace', index=False)
        sql.execute("CREATE UNIQUE INDEX idx_gmi ON gminy (GMI);")
        terc_rodz.to_sql('rodzaje', sql, if_exists='replace', index=False)
        sql.execute("CREATE UNIQUE INDEX idx_rodz ON rodzaje (RODZ);")
        simc = pd.read_csv('data/simc.csv', sep=',', dtype=str)
        simc = simc.loc[:,['SIMC', 'TERC', 'NAZWA_MIEJSCOWOŚCI']].rename(columns={'NAZWA_MIEJSCOWOŚCI': 'NAZWA'})
        simc.to_sql('simc', sql, if_exists='replace', index=False)
        sql.execute("CREATE UNIQUE INDEX idx_simc ON simc (SIMC)")
        sql.execute("CREATE INDEX idx_simc_terc ON simc (TERC)")
        ulic = pd.read_csv('data/ulic.csv', sep=',', dtype=str).rename(columns={'NAZWA': 'nazwa'})
        #ulic['typ'] = ulic['nazwa'].str.split(' ').str[0]
        #ulic['nazwa'] = ulic['nazwa'].str.split(' ').str[1:].str.join(' ')
        ulic.to_sql('ulic', sql, if_exists='replace', index=False)
        sql.execute('CREATE INDEX idx_ulic_simc ON ulic (SIMC)')
        sql.execute('CREATE INDEX idx_ulic ON ulic (ULIC)')
        sql.execute('CREATE UNIQUE INDEX idx_ulic_simc_ulic ON ulic (SIMC, ULIC)')
        budynki_files = glob('./data/[0-9][0-9].csv') 
        for file in budynki_files:
            df = pd.read_csv(file, sep=',', dtype=str)
            df = df.loc[:, ['TERC', 'SIMC', 'SYM_UL', 'Nr budynku', 'Szerokość geograficzna', 'Długość geograficzna']] \
            .rename(columns={'SYM_UL': 'ULIC', 'Nr budynku': 'Nr', 'Szerokość geograficzna': 'lat', 'Długość geograficzna': 'long'})
            df.to_sql('budynki', sql, if_exists='append', index=False)
        sql.execute("CREATE INDEX idx_budynki_terc ON budynki (TERC)")
        sql.execute("CREATE INDEX idx_budynki_simc ON budynki (SIMC)")
        sql.execute("CREATE INDEX idx_budynki_ulic ON budynki (ULIC)")
        sql.execute('CREATE INDEX idx_budynki_simc_ulic ON budynki (SIMC, ULIC)')
        sql.execute("DROP TABLE IF EXISTS klasyfikacje")
        for file in glob(os.path.join("./data/classification-randomized/output", '*.csv')):
            pd.read_csv(file, sep=";", names=["nazwa", "kategoria"]).to_sql('klasyfikacje', sql, if_exists='append', index=False)
        sql.execute("CREATE INDEX idx_klasyfikacje_nazwa ON klasyfikacje (nazwa)")
        sql.execute("ANALYZE;")
        return (sql, cursor)
if __name__ == "__main__":
    parse_data(force=True)
