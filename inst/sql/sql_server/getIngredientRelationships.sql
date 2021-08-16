WITH evidence_summary as (
    {@concept_desc != ''} ? {
    -- Where children of the ingredient concept are explicitly included
    SELECT
        ms.condition_concept_id,
        ms.ingredient_concept_id
    FROM @cem_schema.matrix_summary ms
    INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = ms.ingredient_concept_id)
    WHERE ca.ancestor_concept_id IN (@concept_desc)
    AND ms.evidence_exists = 1
    }

    {@concept_no_desc != ''} ? {
    UNION
    -- Where children of the ingredient concept are explicitly excluded
    SELECT
        ms.condition_concept_id,
        ms.ingredient_concept_id
    FROM @cem_schema.matrix_summary ms
    WHERE ms.condition_concept_id IN (@concept_no_desc)
    AND ms.evidence_exists = 1
    }
)

SELECT {@limit_row_count != ''} ? {TOP @limit_row_count} *
FROM (
SELECT DISTINCT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)
INNER JOIN @vocabulary.concept_ancestor ca2 ON c2.concept_id = ca2.descendant_concept_id
INNER JOIN evidence_summary es ON (cu.concept_id_2 = es.condition_concept_id AND cu.concept_id_1 = es.ingredient_concept_id)
) results;
