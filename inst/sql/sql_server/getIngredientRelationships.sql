SELECT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)
INNER JOIN @vocabulary.concept_ancestor ca1 ON c1.concept_id = ca1.descendant_concept_id
INNER JOIN @vocabulary.concept_ancestor ca2 ON c2.concept_id = ca2.descendant_concept_id
WHERE ca1.descendant_concept_id IN (@ingredient_concepts_desc)

UNION

SELECT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)
INNER JOIN @vocabulary.concept_ancestor ca2 ON c2.concept_id = ca2.descendant_concept_id
WHERE c1.concept_id IN (@ingredient_concepts_no_desc)
