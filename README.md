# sample-nats-architecture

A sample project demonstrating how to make use of queues with [NATS]
in [Kubernetes] using [Haskell] workers.

#### Table of Contents

- [Requirements](#requirements)
- [Getting Started with Minikube](#getting-started-with-minikube)
    - [Quickstart](#quickstart)
    - [Publishing Images to Minikube's Registry](#publishing-images-to-minikubes-registry)
- [Initial NATS Configuration](#initial-nats-configuration)
- [Deploying Workers](#deploying-workers)

## Requirements

Make sure you have the following tools installed:

  - [Docker]
  - [Stack]
  - [Helm]
  - [make]
  - [Minikube] running [Kubernetes] `>= 1.9`

## Getting Started with Minikube

Make sure you have [Minikube] installed and have started a cluster with
`minikube start`. If you're on macOS then you may want to also install
[VirtualBox] and start minikube as `minikube start --hypervisor=virtualbox` for
a more [stable experience].

### Quickstart

You can quickly get started publishing images by running the following
command to expose Minikube's in-cluster registry on your host machine:

```bash
# This should only need to be run once per cluster
make minikube-expose-registry
```

Then, when publishing new versions of the project you can do so by listing
the component and version like so:

```bash
# Build new images for every component of this project
stack image container

# Push the latest images to our Minikube registry
make publish I=sample-producer V=latest
make publish I=sample-consumer V=latest
```

The next section explains what this target is doing.

### Publishing Images to Minikube's Registry

To simplify development you'll want to enable [Minikube]'s registry for
your environment so that images you build locally are available within the
cluster.

```bash
minikube addons enable registry
```

This starts up a [Docker registry] within the cluster but is not exposed to
your host machine by default.

To enable it, we need to [expose] the service and ask [Minikube] to give us the
url that we need to tack to the registry:

```bash
# Expose the registry to our machine
kubectl --namespace kube-system \
  expose service registry \
    --name exposed-registry \
    --type=NodePort \
    --target-port=5000
```

```bash
# Ask Minikube for the URL to talk to our registry
minikube service --namespace kube-system exposed-registry --url
```

In order for us to install images within the cluster using the in-cluster DNS
name we'll want to make sure the `kube-dns` plugin is enabled:

```
minikube addons enable kube-dns
```

Due to [this issue][dns-issue] we'll need to manually update [Minikube]'s
DNS configuration in order to resolve correctly:


```bash
# Record this value, we will need it when modifying Minikube's DNS config
echo $(kubectl get svc kube-dns -n kube-system -o jsonpath='{.spec.clusterIP}')
```

```bash
minikube ssh
sudo vi /etc/systemd/resolved.conf
# Update the file with DNS=${IP_FROM_PREVIOUS_KUBECTL_COMMAND}
sudo systemctl restart systemd-resolved
# Verify DNS is working properly now
nslookup registry.kube-system.svc.cluster.local
exit
```

Next, we'll need to tell Docker about our local registry using the URL
from the previous step. Since [Minikube] is exposing the registry without
TLS we'll need to add an [insecure registry] configuration. For those using
[Docker Community Edition for Mac][dForM] this setting can be changed in
`Preferences > Daemon > Insecure registries`.

We're now ready to publish images to our local registry. We can do so easily
for images in this project with the `tag`, `publish` and `publish` targets:

```bash
# Tag the latest sample-producer image with Minikube's registry information
make tag I=sample-producer V=latest

# Publish it to Minikube's registry
make publish I=sample-producer V=latest

# Combine the previous two steps into one make target
make publish I=sample-producer V=latest
```

The logic behind these targets can be found in `./Makefile`.

## Initial NATS Configuration

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

## Deploying Workers

The workers in this project are built using [Stack] and deployed using [Helm].
The build configuration for containers can be found in [stack.yaml](./stack.yaml)
and the deploy configuration for each worker can be found in the
`{worker}/chart` directory.

The `deploy` make target makes use of [Helm] to upgrade to the latest published
image using the directory name of the work (e.g. `producer` or `consumer`):

```bash
make deploy N=producer
```

You should see logs populate for both workers with messages!

[Docker]: https://www.docker.com/community-edition
[Stack]: https://www.haskellstack.org/
[Helm]: https://helm.sh
[Minikube]: https://github.com/kubernetes/minikube
[Kubernetes]: https://kubernetes.io
[NATS]: https://nats.io/
[crd]: https://kubernetes.io/docs/concepts/extend-kubernetes/api-extension/custom-resources/
[rbac]: https://kubernetes.io/docs/reference/access-authn-authz/rbac/
[nats-operator]: https://github.com/nats-io/nats-operator
[VirtualBox]: https://www.virtualbox.org/wiki/Downloads
[stable experience]: https://blog.datasyndrome.com/docker-on-os-x-hyperkit-not-ready-21c3ca74562a
[make]: https://www.gnu.org/software/make/
[Haskell]: https://www.haskell.org/
[Docker registry]: https://hub.docker.com/_/registry/
[expose]: https://kubernetes.io/docs/tasks/access-application-cluster/service-access-application-cluster/
[insecure registry]: https://docs.docker.com/registry/insecure/
[dForM]: https://store.docker.com/editions/community/docker-ce-desktop-mac
[dns-issue]: https://github.com/kubernetes/minikube/issues/2162
