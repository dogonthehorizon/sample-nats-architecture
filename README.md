# sample-nats-architecture

A sample project demonstrating how to make use of queues with [NATS]
in [Kubernetes] using Haskell.

## Development

Make sure you have the following tools installed:

  - [Docker]
  - [Stack]
  - [Helm]
  - (Optional) [Minikube]

#### Note: Working with Minikube

To simplify development you'll want to enable [Minikube]'s registry for
your development environment so that images are available inside the cluster.

```bash
eval $(minikube docker-env)
```

This also means that images will be **built** inside of the hypervisor that is
hosting your local cluster. Make sure the hypervisor is given enough of the
host's resources so that your Haskell [builds don't take ages][compiling].

### Initial NATS Configuration

We'll use the [nats-operator] project to provision a cluster for us. You can
setup the operator using the provided [RBAC-enabled][rbac] definition in the
`inf` folder:

```bash
kubectl apply -f inf/nats-operator.yaml
```

This will create an operator in the default namespace with the necessary [RBAC]
definitions to be able to create NATS clusters on your behalf. It also creates
a [Custom Resource Definition (CRD)][crd] that we will use to provision our
first cluster:

```bash
kubectl apply -f inf/sample-cluster.yaml
```

[Docker]: https://www.docker.com/community-edition
[Stack]: https://www.haskellstack.org/
[Helm]: https://helm.sh
[Minikube]: https://github.com/kubernetes/minikube
[Kubernetes]: https://kubernetes.io
[NATS]: https://nats.io/
[crd]: https://kubernetes.io/docs/concepts/extend-kubernetes/api-extension/custom-resources/
[rbac]: https://kubernetes.io/docs/reference/access-authn-authz/rbac/
[compiling]: https://xkcd.com/303/
