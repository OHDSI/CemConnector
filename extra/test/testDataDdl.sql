{DEFAULT @cem = 'cem'}

CREATE TABLE @cem.source
(
	source_id VARCHAR(256)
	,description VARCHAR(1000)
	,provenance VARCHAR(256)
	,contributor_organization VARCHAR(256)
	,contact_name VARCHAR(256)
	,creation_date DATE
	,coverage_start_date DATE
	,coverage_end_date DATE
	,version_identifier VARCHAR(256)
)
;


INSERT INTO @cem.source (source_id,description,provenance,contributor_organization,contact_name,creation_date,coverage_start_date,coverage_end_date,version_identifier) VALUES
	 ('AEOLUS','Spontaneous reports and signals from FDA Adverse Event Reporting System (FAERS) based on the paper Banda, J. M. et al. A curated and standardized adverse drug event resource to accelerate drug safety research. Sci. Data 3:160026 doi: 10.1038/sdata.2016.26 (2016).','AEOLUS','Center for Biomedical Informatics Research, Stanford University','Lee Evans (LTS Computing LLC)','2021-01-02','2004-01-01','2020-09-30',NULL),
	 ('COMMONEVIDENCEMODEL','CommonEvidenceModel (CEM) is the infrastructure to pull together public sources of information on drugs and conditions and standardize their format and vocabularies.','COMMONEVIDENCEMODEL','OHDSI','Erica Voss','2020-02-26','1986-01-01','2020-01-23','V2.0.0'),
	 ('EU_PL_ADR','From the PROTECT ADR database, this provided a list of ADRS on Summary of Product Characteristics (SPC) of products authorized in the European Union.  Pharmacoepidemiological Research on Outcomes of Therapeutics by a European Consortium (PROTECT), Adverse Drug Reactions Database, [webpage] (2015.05.07), Available from: <http://www.imi-protect.eu/adverseDrugReactions.shtml>','EU_PL_ADR','PROTECT','PROTECT','2020-12-20','1900-01-01','2017-06-30','20170630'),
	 ('MEDLINE_AVILLACH','Co-occurrence of a drug and condition MeSH tag on a publication with the qualifiers "adverse effects" and "chemically induced" respectively.  Based on publication Avillach P, Dufour JC, Diallo G, Salvo F, Joubert M, Thiessard F, Mougin F, Trifir? G, Fourrier-R?glat A, Pariente A, Fieschi M. Design and validation of an automated method to detect known adverse drug reactions in MEDLINE: a contribution from the EU-ADR project. J Am Med Inform Assoc. 2013 May 1;20(3):446-52. doi: 10.1136/amiajnl-2012-001083. Epub 2012 Nov 29. PubMed PMID: 23195749; PubMed Central PMCID: PMC3628051.','MEDLINE','Janssen R&D','Erica Voss','2020-02-09','1900-01-01','2018-05-28',NULL),
	 ('MEDLINE_PUBMED','Co-occurrence of a drug and condition MeSH tag or found in the Title of Abstract of a publication.  Leverages Pubmed.','PUBMED','Janssen R&D','Erica Voss','2020-02-19','1990-01-01','2018-05-28',NULL),
	 ('MEDLINE_WINNENBURG','Winnenburg R, Sorbello A, Ripple A, Harpaz R, Tonning J, Szarfman A, Francis H, Bodenreider O. Leveraging MEDLINE indexing for pharmacovigilance - Inherent limitations and mitigation strategies. J Biomed Inform. 2015 Oct;57:425-35. doi:10.1016-j.jbi.2015.08.022. Epub 2015 Sep 2. PubMed PMID: 26342964; PubMed Central PMCID: PMC4775467.','MEDLINE','Janssen R&D','Erica Voss','2020-02-19','2020-01-01','2018-05-28',NULL),
	 ('OMOP_VOCABULARY','OMOP Vocabulary','VOCABULARY','OHDSI','Odysseus','2020-08-12','2018-01-26','2020-06-11','v5.0 06-NOV-20'),
	 ('SEMMEDDB','Semantic Medline uses natural language processing to extract semantic predictions from titles and text.  "H. Kilicoglu et al., Constructing a semantic predication gold standard from the biomedical literature, BMC Bioinformatics 12 (2011) 486."','SEMMEDDB','National Institutes of Health','National Institutes of Health','2020-12-20','1865-01-01','2020-08-27','semmedVER43_R'),
	 ('SHERLOCK','ClinicalTrials.gov publicly makes available information about clinical trials and is maintained by the U.S. National Library of Medicine (NLM) and the U.S. National Institutes of Health (NIH). Each trial, however, comes as an individual eXtensible Markup Language (XML) file which is difficult for summarizing information across trials. Instead of using the XMLs we leverage a tool called Sherlock which downloads trial information from ClinicalTrials.gov, and then parses and organizes that data for analysis. For more information on Sherlock:  Cepeda, M.S., V. Lobanov, and J.A. Berlin, Use of ClinicalTrials.gov to estimate condition-specific nocebo effects and other factors affecting outcomes of analgesic trials. J Pain, 2013. 14(4): p. 405-11.','SHERLOCK','Janssen R&D','Erica Voss','2020-01-27','1931-06-01','2020-01-23',NULL),
	 ('SPLICER','Adverse drug reactions extracted from the Adverse Reactions or Post Marketing section of United States product labeling. Basd on publication J. Duke, J. Friedlin, X. Li, Consistency in the safety labeling of bioequivalent medications, Pharmacoepidemiol. Drug Saf. 22 (3) (2013) 294?301.','SPLICER','Georgia Tech','Jon Duke','2019-12-08','1990-12-01','2019-12-08',NULL);


CREATE TABLE @cem.cem_unified
(
    id INTEGER DEFAULT 0
	,concept_id_1 INTEGER
	,source_code_1 VARCHAR(255)
	,source_code_type_1 VARCHAR(50)
	,concept_id_2 INTEGER
	,source_code_2 VARCHAR(255)
	,source_code_type_2 VARCHAR(50)
	,source_id VARCHAR(50)
	,evidence_type VARCHAR(50)
	,relationship_id VARCHAR(20)
	,statistic_value NUMERIC
	,statistic_value_type VARCHAR(255)
	,unique_identifier VARCHAR(500)
	,unique_identifier_type VARCHAR(255)
	,count_how VARCHAR(255)
);

CREATE TABLE @cem.matrix_summary
(
    INGREDIENT_CONCEPT_ID INTEGER NOT NULL,
    CONDITION_CONCEPT_ID INTEGER NOT NULL,
    EVIDENCE_EXISTS INTEGER NOT NULL
);