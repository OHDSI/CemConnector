WITH evidence_summary as (
    -- Where children of the ingredient concept are explicitly included
    SELECT
        ms.ingredient_concept_id,
        ms.condition_concept_id
    FROM @cem_schema.matrix_summary ms
    INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = ms.condition_concept_id)
    WHERE ca.ancestor_concept_id IN (@condition_concept_desc)
    AND ms.evidence_exists = 1
    -- Where children of the ingredient concept are explicitly excluded
    {@condition_concept_no_desc != ''} ? {
    UNION

    SELECT
        ms.ingredient_concept_id,
        ms.condition_concept_id
    FROM @cem_schema.matrix_summary ms
    WHERE ms.condition_concept_id IN (@condition_concept_no_desc)
    AND ms.evidence_exists = 1
    }

    {@use_siblings} ? {
    UNION

    SELECT
        ms.ingredient_concept_id,
        ms.condition_concept_id
    FROM @cem_schema.matrix_summary ms
    INNER JOIN @vocabulary.concept_ancestor ca1 ON (ca1.descendant_concept_id = ms.condition_concept_id)
    INNER JOIN @vocabulary.concept_ancestor ca2 ON (ca2.ancestor_concept_id = ca1.ancestor_concept_id AND ca2.max_levels_of_separation <= @sibling_lookup_levels)
    WHERE ca2.descendant_concept_id IN (@condition_concept_desc)
    AND ms.evidence_exists = 1
    }
)

SELECT
DISTINCT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)
INNER JOIN @vocabulary.concept_ancestor ca2 ON c2.concept_id = ca2.descendant_concept_id
INNER JOIN evidence_summary es ON (cu.concept_id_2 = es.condition_concept_id AND cu.concept_id_1 = es.ingredient_concept_id);