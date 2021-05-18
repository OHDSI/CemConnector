-- Where children of the ingredient concept are explicitly included
SELECT
    ms.ingredient_concept_id,
    c.concept_name,
    max(ms.evidence_exists) as evidence_exists
FROM @cem_schema.matrix_summary ms
INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = ms.condition_concept_id)
INNER JOIN @vocabulary.concept c ON (c.concept_id = ms.ingredient_concept_id)
WHERE ca.ancestor_concept_id IN (@condition_concept_desc)
GROUP BY c.concept_name,  ms.ingredient_concept_id
-- Where children of the ingredient concept are explicitly excluded
{@condition_concept_no_desc != ''} ? {
UNION

SELECT
    ms.ingredient_concept_id,
    c.concept_name,
    max(ms.evidence_exists) as evidence_exists
FROM @cem_schema.matrix_summary ms
INNER JOIN @vocabulary.concept c ON (c.concept_id = ms.ingredient_concept_id)
WHERE ms.condition_concept_id IN (@condition_concept_no_desc)
GROUP BY cs.conceptset_id, c.concept_name,  ms.ingredient_concept_id
}