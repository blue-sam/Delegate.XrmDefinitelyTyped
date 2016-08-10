using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Xrm.Sdk;
using Microsoft.Xrm.Sdk.Messages;
using Microsoft.Xrm.Sdk.Metadata;
using Microsoft.Xrm.Sdk.Query;

namespace XrmDefinitelyTyped.Metadata
{
    public static class GetSolutionEntities
    {
        public static EntityMetadata[] GetSolutionMetadata(IOrganizationService orgService, string[] solutionNames)
        {
            var allEntities = GetAllEntities(orgService);
            return solutionNames.SelectMany(s =>
            {
                var entityIds = GetSolutionEntitiesInt(s, orgService);
                return allEntities.Where(e => (e.MetadataId.HasValue && entityIds.Contains(e.MetadataId.Value))).SelectMany(
                    e =>
                    {
                        var toReturn = new List<string>() {e.LogicalName};
                        toReturn.AddRange(e.ManyToManyRelationships.Select(r => r.IntersectEntityName));
                        toReturn.AddRange(e.OneToManyRelationships.Select(r => r.ReferencingEntity));
                        toReturn.AddRange(e.ManyToOneRelationships.Select(r => r.ReferencedEntity));
                        return toReturn;
                    });
            }).Distinct().Select(e => allEntities.First(a => a.LogicalName == e)).ToArray();
        }


        public static EntityMetadata[] GetAllEntities(IOrganizationService Service)
        {
            //Get all entities
            RetrieveAllEntitiesRequest AllEntitiesrequest = new RetrieveAllEntitiesRequest()
            {
            };
            RetrieveAllEntitiesResponse AllEntitiesresponse = (RetrieveAllEntitiesResponse)Service.Execute(AllEntitiesrequest);
            var result = AllEntitiesresponse.EntityMetadata.Select(e =>
            {
                var entityRequest = new RetrieveEntityRequest { LogicalName = e.LogicalName, EntityFilters = EntityFilters.All };
                var temp = (RetrieveEntityResponse)Service.Execute(entityRequest);
                return temp.EntityMetadata;
            });
            return result.ToArray();
        }

        static List<Guid> GetSolutionEntitiesInt(string SolutionUniqueName, IOrganizationService Service)
        {
            // get solution components for solution unique name
            QueryExpression componentsQuery = new QueryExpression
            {
                EntityName = "solutioncomponent",
                ColumnSet = new ColumnSet("objectid"),
                Criteria = new FilterExpression(),
            };
            LinkEntity solutionLink = new LinkEntity("solutioncomponent", "solution", "solutionid", "solutionid", JoinOperator.Inner);
            solutionLink.LinkCriteria = new FilterExpression();
            solutionLink.LinkCriteria.AddCondition(new ConditionExpression("uniquename", ConditionOperator.Equal, SolutionUniqueName));
            componentsQuery.LinkEntities.Add(solutionLink);
            componentsQuery.Criteria.AddCondition(new ConditionExpression("componenttype", ConditionOperator.Equal, 1));
            EntityCollection ComponentsResult = Service.RetrieveMultiple(componentsQuery);
            return ComponentsResult.Entities.Select(x => x.Attributes["objectid"] as Guid?).Where(x => x != null).Select(s => s.Value).ToList();
        }
    }
}
